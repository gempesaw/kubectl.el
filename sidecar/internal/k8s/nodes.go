package k8s

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"time"

	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/watch"
)

type NodeUtilization struct {
	Name     string
	CPUMilli int64
	MemMi    int64
}

// FetchNodeMetrics gets per-node CPU/mem usage from metrics.k8s.io/v1beta1.
func (c *Clients) FetchNodeMetrics(ctx context.Context) ([]NodeUtilization, error) {
	list, err := c.Metrics.MetricsV1beta1().NodeMetricses().List(ctx, metav1.ListOptions{})
	if err != nil {
		return nil, fmt.Errorf("list node metrics: %w", err)
	}
	out := make([]NodeUtilization, 0, len(list.Items))
	for _, nm := range list.Items {
		var cpu, mem int64
		if v, ok := nm.Usage[corev1.ResourceCPU]; ok {
			cpu = v.MilliValue()
		}
		if v, ok := nm.Usage[corev1.ResourceMemory]; ok {
			mem = memToMi(v)
		}
		out = append(out, NodeUtilization{Name: nm.Name, CPUMilli: cpu, MemMi: mem})
	}
	return out, nil
}

// NodeAggregate is the per-node roll-up of pod-level data: how many pods are
// running on the node, how many GPUs they collectively claim, and the sum of
// their CPU and memory requests.
type NodeAggregate struct {
	NodeName string
	PodCount int
	GPUCount int
	CPUMilli int64
	MemMi    int64
}

// podStat is the per-pod data the node aggregator caches between events.
type podStat struct {
	nodeName string
	cpuMilli int64
	memMi    int64
	gpus     int
}

func computePodStat(pod *corev1.Pod) podStat {
	s := podStat{nodeName: pod.Spec.NodeName}
	for _, ctr := range pod.Spec.Containers {
		if v, ok := ctr.Resources.Requests[corev1.ResourceCPU]; ok {
			s.cpuMilli += v.MilliValue()
		}
		if v, ok := ctr.Resources.Requests[corev1.ResourceMemory]; ok {
			s.memMi += memToMi(v)
		}
		if v, ok := ctr.Resources.Limits["nvidia.com/gpu"]; ok {
			s.gpus += int(v.Value())
		}
		if v, ok := ctr.Resources.Limits["amd.com/gpu"]; ok {
			s.gpus += int(v.Value())
		}
	}
	return s
}

// StreamPodAggregates opens a cluster-wide watch on Running pods and emits a fresh
// per-node aggregate map on every pod change. Initial state is published as soon
// as the first LIST returns. Aggregator self-recovers on watch errors (re-LIST and
// re-WATCH). Channel closes only when the context is cancelled.
//
// This replaces the previous "LIST every 15s" polling so the node table's
// Pods/CReq/MReq/GPUs columns track pod churn in near-real-time.
func (c *Clients) StreamPodAggregates(ctx context.Context) <-chan map[string]*NodeAggregate {
	out := make(chan map[string]*NodeAggregate, 4)

	go func() {
		defer close(out)
		pods := make(map[string]podStat) // key: ns/name

		publish := func() {
			agg := make(map[string]*NodeAggregate, 64)
			for _, p := range pods {
				if p.nodeName == "" {
					continue
				}
				a, ok := agg[p.nodeName]
				if !ok {
					a = &NodeAggregate{NodeName: p.nodeName}
					agg[p.nodeName] = a
				}
				a.PodCount++
				a.CPUMilli += p.cpuMilli
				a.MemMi += p.memMi
				a.GPUCount += p.gpus
			}
			select {
			case out <- agg:
			case <-ctx.Done():
			}
		}

		for ctx.Err() == nil {
			// Initial LIST — trims to Running pods on the server side so we don't
			// stream Pending/Succeeded/Failed pods we'd ignore anyway.
			list, err := c.Core.CoreV1().Pods("").List(ctx, metav1.ListOptions{
				FieldSelector: "status.phase==Running",
			})
			if err != nil {
				log.Printf("[node-agg] list cluster pods: %v", err)
				if !sleepCtx(ctx, 2*time.Second) {
					return
				}
				continue
			}

			pods = make(map[string]podStat, len(list.Items))
			for i := range list.Items {
				p := &list.Items[i]
				pods[p.Namespace+"/"+p.Name] = computePodStat(p)
			}
			publish()

			// Stream changes from the LIST's resourceVersion.
			w, err := c.Core.CoreV1().Pods("").Watch(ctx, metav1.ListOptions{
				FieldSelector:       "status.phase==Running",
				ResourceVersion:     list.ResourceVersion,
				AllowWatchBookmarks: true,
			})
			if err != nil {
				log.Printf("[node-agg] watch: %v", err)
				if !sleepCtx(ctx, 2*time.Second) {
					return
				}
				continue
			}

			for ev := range w.ResultChan() {
				switch ev.Type {
				case watch.Added, watch.Modified:
					p, ok := ev.Object.(*corev1.Pod)
					if !ok {
						continue
					}
					pods[p.Namespace+"/"+p.Name] = computePodStat(p)
					publish()
				case watch.Deleted:
					p, ok := ev.Object.(*corev1.Pod)
					if !ok {
						continue
					}
					delete(pods, p.Namespace+"/"+p.Name)
					publish()
				case watch.Error:
					log.Printf("[node-agg] watch error event: %v", ev.Object)
				}
			}
			// Result channel closed (server timeout / disconnect). Loop and re-LIST.
		}
	}()

	return out
}

// sleepCtx is like time.Sleep but bails out on ctx cancellation. Returns false if
// the context was cancelled.
func sleepCtx(ctx context.Context, d time.Duration) bool {
	select {
	case <-time.After(d):
		return true
	case <-ctx.Done():
		return false
	}
}

// DecodeNode unpacks a Row's Object as a *corev1.Node.
func DecodeNode(r Row) (*corev1.Node, error) {
	if len(r.Object) == 0 {
		return nil, nil
	}
	var node corev1.Node
	if err := json.Unmarshal(r.Object, &node); err != nil {
		return nil, err
	}
	return &node, nil
}

// NodeAllocatable returns the node's allocatable CPU (millicores) and memory (Mi).
// These are the denominators for the per-node CReq/CUse/MReq/MUse percentage columns.
func NodeAllocatable(node *corev1.Node) (cpuMilli int64, memMi int64) {
	if v, ok := node.Status.Allocatable[corev1.ResourceCPU]; ok {
		cpuMilli = v.MilliValue()
	}
	if v, ok := node.Status.Allocatable[corev1.ResourceMemory]; ok {
		memMi = memToMi(v)
	}
	return
}

// NodeGPUTotal returns the total GPU count (nvidia + amd) the node has allocatable.
// Returns 0 for nodes without GPUs.
func NodeGPUTotal(node *corev1.Node) int {
	total := int64(0)
	if v, ok := node.Status.Allocatable["nvidia.com/gpu"]; ok {
		total += v.Value()
	}
	if v, ok := node.Status.Allocatable["amd.com/gpu"]; ok {
		total += v.Value()
	}
	return int(total)
}

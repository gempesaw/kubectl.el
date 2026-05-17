package k8s

import (
	"context"
	"encoding/json"
	"fmt"

	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
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

// FetchClusterPodAggregates lists all Running pods cluster-wide and groups by node.
// One LIST per call (matches python's poll_podcount cadence). The field selector
// trims completed pods on the server side.
func (c *Clients) FetchClusterPodAggregates(ctx context.Context) (map[string]*NodeAggregate, error) {
	list, err := c.Core.CoreV1().Pods("").List(ctx, metav1.ListOptions{
		FieldSelector: "status.phase==Running",
	})
	if err != nil {
		return nil, fmt.Errorf("list cluster pods: %w", err)
	}

	out := make(map[string]*NodeAggregate)
	for i := range list.Items {
		pod := &list.Items[i]
		node := pod.Spec.NodeName
		if node == "" {
			continue
		}
		agg, ok := out[node]
		if !ok {
			agg = &NodeAggregate{NodeName: node}
			out[node] = agg
		}
		agg.PodCount++
		for _, ctr := range pod.Spec.Containers {
			if v, ok := ctr.Resources.Requests[corev1.ResourceCPU]; ok {
				agg.CPUMilli += v.MilliValue()
			}
			if v, ok := ctr.Resources.Requests[corev1.ResourceMemory]; ok {
				agg.MemMi += memToMi(v)
			}
			if v, ok := ctr.Resources.Limits["nvidia.com/gpu"]; ok {
				agg.GPUCount += int(v.Value())
			}
			if v, ok := ctr.Resources.Limits["amd.com/gpu"]; ok {
				agg.GPUCount += int(v.Value())
			}
		}
	}
	return out, nil
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

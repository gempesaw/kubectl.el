package k8s

import (
	"context"
	"fmt"

	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

// PodUtilization is the current CPU/memory usage summed across a pod's containers,
// in the same units we use for requests/limits aggregation (millicores, Mi).
type PodUtilization struct {
	Name      string
	Namespace string
	CPUMilli  int64
	MemMi     int64
}

// FetchPodMetrics queries metrics.k8s.io for current usage. If metrics-server is
// unavailable, returns nil without error so we degrade gracefully (matches the
// watch.py behavior of leaving the util columns blank when kube-capacity errors).
func (c *Clients) FetchPodMetrics(ctx context.Context, namespace string) ([]PodUtilization, error) {
	listIface := c.Metrics.MetricsV1beta1().PodMetricses(namespace)
	if namespace == "" {
		listIface = c.Metrics.MetricsV1beta1().PodMetricses(metav1.NamespaceAll)
	}
	list, err := listIface.List(ctx, metav1.ListOptions{})
	if err != nil {
		return nil, fmt.Errorf("list pod metrics: %w", err)
	}

	out := make([]PodUtilization, 0, len(list.Items))
	for _, pm := range list.Items {
		var cpu, mem int64
		for _, c := range pm.Containers {
			if v, ok := c.Usage[corev1.ResourceCPU]; ok {
				cpu += v.MilliValue()
			}
			if v, ok := c.Usage[corev1.ResourceMemory]; ok {
				mem += memToMi(v)
			}
		}
		out = append(out, PodUtilization{
			Name:      pm.Name,
			Namespace: pm.Namespace,
			CPUMilli:  cpu,
			MemMi:     mem,
		})
	}
	return out, nil
}

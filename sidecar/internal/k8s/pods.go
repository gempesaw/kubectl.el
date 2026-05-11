package k8s

import (
	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/resource"
)

// SumContainerRequests returns total CPU (millicores) and memory (Mi) requests across
// all containers (mirroring kube-capacity's accounting).
func SumContainerRequests(pod *corev1.Pod) (cpuMilli int64, memMi int64) {
	for _, c := range pod.Spec.Containers {
		if r, ok := c.Resources.Requests[corev1.ResourceCPU]; ok {
			cpuMilli += r.MilliValue()
		}
		if r, ok := c.Resources.Requests[corev1.ResourceMemory]; ok {
			memMi += memToMi(r)
		}
	}
	return
}

// SumContainerLimits is the same shape as SumContainerRequests for limits.
func SumContainerLimits(pod *corev1.Pod) (cpuMilli int64, memMi int64) {
	for _, c := range pod.Spec.Containers {
		if r, ok := c.Resources.Limits[corev1.ResourceCPU]; ok {
			cpuMilli += r.MilliValue()
		}
		if r, ok := c.Resources.Limits[corev1.ResourceMemory]; ok {
			memMi += memToMi(r)
		}
	}
	return
}

func memToMi(q resource.Quantity) int64 {
	// Quantity.Value() returns bytes; convert to Mi (1Mi = 1024*1024 bytes).
	return q.Value() / (1024 * 1024)
}

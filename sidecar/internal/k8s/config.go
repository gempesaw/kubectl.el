package k8s

import (
	"fmt"

	"k8s.io/client-go/kubernetes"
	"k8s.io/client-go/rest"
	"k8s.io/client-go/tools/clientcmd"
	metricsclientset "k8s.io/metrics/pkg/client/clientset/versioned"
)

type Clients struct {
	Core    kubernetes.Interface
	Metrics metricsclientset.Interface
	REST    rest.Interface
}

// Load builds clients from the user's kubeconfig (current context + exec auth handled
// automatically). No explicit AWS env juggling — `aws eks get-token` exec plugins
// inherit the surrounding environment, which is what we want.
func Load() (*Clients, error) {
	loading := clientcmd.NewDefaultClientConfigLoadingRules()
	cfg, err := clientcmd.NewNonInteractiveDeferredLoadingClientConfig(loading, &clientcmd.ConfigOverrides{}).ClientConfig()
	if err != nil {
		return nil, fmt.Errorf("load kubeconfig: %w", err)
	}

	core, err := kubernetes.NewForConfig(cfg)
	if err != nil {
		return nil, fmt.Errorf("build core client: %w", err)
	}

	metrics, err := metricsclientset.NewForConfig(cfg)
	if err != nil {
		return nil, fmt.Errorf("build metrics client: %w", err)
	}

	return &Clients{
		Core:    core,
		Metrics: metrics,
		REST:    core.CoreV1().RESTClient(),
	}, nil
}

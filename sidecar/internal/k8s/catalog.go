package k8s

import (
	"context"
	"fmt"
	"strings"
	"sync"

	"k8s.io/apimachinery/pkg/runtime/schema"
)

// staticResourceCatalog seeds the alias→ResourceID table with the common kinds we
// already know about. Discovery merges any cluster-specific kinds on top (CRDs,
// non-standard API groups). Static entries win on conflict so common short aliases
// stay mapped to the canonical resource.
var staticResourceCatalog = map[string]ResourceID{
	"po":     {Alias: "po", Plural: "pods", Group: "", Version: "v1", Namespaced: true, KindPrefix: "pod"},
	"pod":    {Alias: "pod", Plural: "pods", Group: "", Version: "v1", Namespaced: true, KindPrefix: "pod"},
	"pods":   {Alias: "pods", Plural: "pods", Group: "", Version: "v1", Namespaced: true, KindPrefix: "pod"},
	"ds":     {Alias: "ds", Plural: "daemonsets", Group: "apps", Version: "v1", Namespaced: true, KindPrefix: "daemonset.apps"},
	"sts":    {Alias: "sts", Plural: "statefulsets", Group: "apps", Version: "v1", Namespaced: true, KindPrefix: "statefulset.apps"},
	"deploy": {Alias: "deploy", Plural: "deployments", Group: "apps", Version: "v1", Namespaced: true, KindPrefix: "deployment.apps"},
	"svc":    {Alias: "svc", Plural: "services", Group: "", Version: "v1", Namespaced: true, KindPrefix: "service"},
	"ing":    {Alias: "ing", Plural: "ingresses", Group: "networking.k8s.io", Version: "v1", Namespaced: true, KindPrefix: "ingress.networking.k8s.io"},
	"cm":     {Alias: "cm", Plural: "configmaps", Group: "", Version: "v1", Namespaced: true, KindPrefix: "configmap"},
}

// NodeResource is the cluster-scoped node ResourceID. Used unconditionally by
// runNodeLoop — not exposed via the alias catalog because nodes aren't part of
// the user's resource list, they're always on.
var NodeResource = ResourceID{
	Alias:      "nodes",
	Plural:     "nodes",
	Group:      "",
	Version:    "v1",
	Namespaced: false,
	KindPrefix: "node",
}

var (
	catalogMu      sync.RWMutex
	catalog        map[string]ResourceID
	catalogSeeded  bool
)

func initCatalogLocked() {
	if catalogSeeded {
		return
	}
	catalog = make(map[string]ResourceID, len(staticResourceCatalog))
	for k, v := range staticResourceCatalog {
		catalog[k] = v
	}
	catalogSeeded = true
}

func LookupResource(alias string) (ResourceID, bool) {
	catalogMu.RLock()
	defer catalogMu.RUnlock()
	if !catalogSeeded {
		catalogMu.RUnlock()
		catalogMu.Lock()
		initCatalogLocked()
		catalogMu.Unlock()
		catalogMu.RLock()
	}
	id, ok := catalog[alias]
	return id, ok
}

func IsPod(alias string) bool {
	return alias == "po" || alias == "pod" || alias == "pods"
}

// PopulateFromDiscovery queries the apiserver for available API resources and merges
// them into the catalog. Each resource is reachable by its plural name, singular
// name, and any short names defined on it. Static entries win on conflict.
//
// Returns the count of new aliases learned. A partial-error from the discovery API
// (common when aggregated APIs are misbehaving) is returned alongside the data — the
// caller can log and continue.
func (c *Clients) PopulateFromDiscovery(ctx context.Context) (int, error) {
	lists, derr := c.Core.Discovery().ServerPreferredResources()
	// ServerPreferredResources can return a partial-success error; lists is usable.

	catalogMu.Lock()
	defer catalogMu.Unlock()
	if !catalogSeeded {
		initCatalogLocked()
	}

	learned := 0
	for _, list := range lists {
		if list == nil {
			continue
		}
		gv, err := schema.ParseGroupVersion(list.GroupVersion)
		if err != nil {
			continue
		}
		for _, r := range list.APIResources {
			// Skip subresources ("pods/log", "pods/status", etc.).
			if strings.Contains(r.Name, "/") {
				continue
			}

			rid := ResourceID{
				Alias:      r.Name,
				Plural:     r.Name,
				Group:      gv.Group,
				Version:    gv.Version,
				Namespaced: r.Namespaced,
				KindPrefix: buildKindPrefix(r.SingularName, r.Kind, gv.Group),
			}

			for _, alias := range append([]string{r.Name, r.SingularName}, r.ShortNames...) {
				if alias == "" {
					continue
				}
				if _, exists := staticResourceCatalog[alias]; exists {
					continue // static wins
				}
				if _, exists := catalog[alias]; exists {
					continue // first discovery wins (preferred version)
				}
				catalog[alias] = rid
				learned++
			}
		}
	}
	if derr != nil {
		return learned, fmt.Errorf("partial discovery: %w", derr)
	}
	return learned, nil
}

// buildKindPrefix matches kubectl's `--show-kind=true` row prefix: "<singular>" for
// core resources, "<singular>.<group>" otherwise. Falls back to the lowercase Kind
// if singular name is empty (some CRDs omit it).
func buildKindPrefix(singular, kind, group string) string {
	prefix := singular
	if prefix == "" {
		prefix = strings.ToLower(kind)
	}
	if group != "" {
		prefix = prefix + "." + group
	}
	return prefix
}

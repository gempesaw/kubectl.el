package k8s

// Catalog of resource aliases this binary knows how to watch. Keep in sync with the
// elisp side's go-handled list — anything missing here will be skipped (and python
// gets to handle it instead, in coexist mode).
var resourceCatalog = map[string]ResourceID{
	"po":     {Alias: "po", Plural: "pods", Group: "", Version: "v1", Namespaced: true},
	"pod":    {Alias: "pod", Plural: "pods", Group: "", Version: "v1", Namespaced: true},
	"pods":   {Alias: "pods", Plural: "pods", Group: "", Version: "v1", Namespaced: true},
	"ds":     {Alias: "ds", Plural: "daemonsets", Group: "apps", Version: "v1", Namespaced: true},
	"sts":    {Alias: "sts", Plural: "statefulsets", Group: "apps", Version: "v1", Namespaced: true},
	"deploy": {Alias: "deploy", Plural: "deployments", Group: "apps", Version: "v1", Namespaced: true},
	"svc":    {Alias: "svc", Plural: "services", Group: "", Version: "v1", Namespaced: true},
	"ing":    {Alias: "ing", Plural: "ingresses", Group: "networking.k8s.io", Version: "v1", Namespaced: true},
	"cm":     {Alias: "cm", Plural: "configmaps", Group: "", Version: "v1", Namespaced: true},
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
}

func LookupResource(alias string) (ResourceID, bool) {
	id, ok := resourceCatalog[alias]
	return id, ok
}

func IsPod(alias string) bool {
	return alias == "po" || alias == "pod" || alias == "pods"
}

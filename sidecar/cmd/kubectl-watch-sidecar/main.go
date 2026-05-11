package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/gempesaw/kubectl.el/sidecar/internal/k8s"
	"github.com/gempesaw/kubectl.el/sidecar/internal/render"
	"github.com/gempesaw/kubectl.el/sidecar/internal/socket"
)

const (
	allNamespacesArg     = "All Namespaces"
	bufferPrefix         = " kubectl--resource-buffer-"
	nodeBufferAlias      = "kcnodes"
	displayLimit         = 20
	metricsInterval      = 10 * time.Second
	nodeMetricsInterval  = 60 * time.Second
	nodeAggregateInterval = 15 * time.Second
	relistBackoff        = 2 * time.Second
)

// nodeLabelColumns are the labels we render as columns at the end of the node table,
// mirroring kubectl's --label-columns behavior. Order matters; matches python's list.
var nodeLabelColumns = []struct {
	Header string
	Key    string
}{
	{"INSTANCE-TYPE", "node.kubernetes.io/instance-type"},
	{"ZONE", "topology.kubernetes.io/zone"},
	{"NODEPOOL", "karpenter.sh/nodepool"},
	{"CAPACITY-TYPE", "karpenter.sh/capacity-type"},
}

// Flags carries the argv-derived knobs. Argv contract matches watch.py:
//   <resources> <namespace> <sort_column> <grep_needle>
// plus KUBECTL_WATCH_SOCKET env.
type Flags struct {
	Resources  []string
	Namespace  string
	SortColumn string
	Grep       string
	SocketPath string
}

func parseFlags() (*Flags, error) {
	if len(os.Args) < 5 {
		return nil, fmt.Errorf("usage: kubectl-watch-sidecar <resources> <namespace> <sort_column> <grep_needle>")
	}
	socketPath := os.Getenv("KUBECTL_WATCH_SOCKET")
	if socketPath == "" {
		return nil, fmt.Errorf("KUBECTL_WATCH_SOCKET not set")
	}
	return &Flags{
		Resources:  strings.Split(os.Args[1], ","),
		Namespace:  os.Args[2],
		SortColumn: os.Args[3],
		Grep:       os.Args[4],
		SocketPath: socketPath,
	}, nil
}

func (f *Flags) IsAllNamespaces() bool {
	return f.Namespace == allNamespacesArg
}

func (f *Flags) EffectiveNamespace() string {
	if f.IsAllNamespaces() {
		return ""
	}
	return f.Namespace
}

func main() {
	flags, err := parseFlags()
	if err != nil {
		log.Fatalf("flag error: %v", err)
	}

	ctx, cancel := signalContext()
	defer cancel()

	clients, err := k8s.Load()
	if err != nil {
		log.Fatalf("k8s clients: %v", err)
	}

	sock := socket.New(flags.SocketPath)
	defer sock.Close()

	// Resolve each requested alias to a ResourceID. Skip unknown ones.
	type subscription struct {
		alias string
		id    k8s.ResourceID
	}
	var subs []subscription
	for _, alias := range flags.Resources {
		if alias == "" {
			continue
		}
		id, ok := k8s.LookupResource(alias)
		if !ok {
			log.Printf("unknown resource alias %q (skipping)", alias)
			continue
		}
		subs = append(subs, subscription{alias: alias, id: id})
	}

	log.Printf("kubectl-watch-sidecar: ns=%q sort=%q grep=%q resources=%v (watching)",
		flags.Namespace, flags.SortColumn, flags.Grep,
		func() []string {
			out := make([]string, len(subs))
			for i, s := range subs {
				out[i] = s.alias
			}
			return out
		}())

	var wg sync.WaitGroup
	for _, sub := range subs {
		wg.Add(1)
		bufferName := bufferPrefix + sub.alias
		if k8s.IsPod(sub.alias) {
			go func(id k8s.ResourceID, buf string) {
				defer wg.Done()
				runPodLoop(ctx, clients, sock, flags, id, buf)
			}(sub.id, bufferName)
		} else {
			go func(id k8s.ResourceID, buf string) {
				defer wg.Done()
				runGenericLoop(ctx, clients, sock, flags, id, buf)
			}(sub.id, bufferName)
		}
	}

	// Nodes are unconditional — the dashboard's kcnodes section always wants them.
	wg.Add(1)
	go func() {
		defer wg.Done()
		runNodeLoop(ctx, clients, sock, flags, bufferPrefix+nodeBufferAlias)
	}()

	wg.Wait()
}

func signalContext() (context.Context, context.CancelFunc) {
	ctx, cancel := context.WithCancel(context.Background())
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-ch
		cancel()
	}()
	return ctx, cancel
}

// runGenericLoop drives a non-pod resource: server-side Table → state map → render.
// No augmentation, no metrics — just the cells the apiserver gives us.
func runGenericLoop(ctx context.Context, clients *k8s.Clients, sock *socket.Client, flags *Flags, id k8s.ResourceID, bufferName string) {
	ns := flags.EffectiveNamespace()

	rows := map[string]k8s.Row{}
	var columns []k8s.TableColumn

	send := func() {
		out := render.Render(buildGenericTable(columns, rows, id.Plural), render.Options{
			Total:       len(rows),
			Grep:        flags.Grep,
			SortColumn:  flags.SortColumn,
			ReverseSort: flags.SortColumn != "AGE",
			Limit:       displayLimit,
		})
		if err := sock.Send(bufferName, out); err != nil {
			log.Printf("[%s] send: %v", id.Plural, err)
		}
	}

	for ctx.Err() == nil {
		snap, err := clients.ListResource(ctx, id, ns)
		if err != nil {
			log.Printf("[%s] list: %v", id.Plural, err)
			sleep(ctx, relistBackoff)
			continue
		}

		columns = snap.Columns
		rows = make(map[string]k8s.Row, len(snap.Rows))
		for _, r := range snap.Rows {
			rows[rowKey(r)] = r
		}
		send()

		events, errs := clients.WatchResource(ctx, id, ns, snap.ResourceVersion)
		if !drainEvents(ctx, events, errs, rows, send) {
			return // ctx cancelled
		}
	}
}

// drainEvents reads from events/errs until either closes. Returns false if ctx ended.
func drainEvents(ctx context.Context, events <-chan k8s.Event, errs <-chan error, rows map[string]k8s.Row, send func()) bool {
	for {
		select {
		case <-ctx.Done():
			return false
		case ev, ok := <-events:
			if !ok {
				return true
			}
			k := rowKey(ev.Row)
			switch ev.Type {
			case "ADDED", "MODIFIED":
				rows[k] = ev.Row
			case "DELETED":
				delete(rows, k)
			}
			send()
		case err, ok := <-errs:
			if !ok {
				return true
			}
			if err != nil {
				log.Printf("watch: %v", err)
			}
			return true
		}
	}
}

// runPodLoop is the pod-specialized watcher: same generic List/Watch + a metrics
// poller running in parallel + a row-augmentation pass that prepends the 6 metric
// columns to each row.
func runPodLoop(ctx context.Context, clients *k8s.Clients, sock *socket.Client, flags *Flags, id k8s.ResourceID, bufferName string) {
	ns := flags.EffectiveNamespace()

	rows := map[string]k8s.Row{}
	var columns []k8s.TableColumn
	metrics := map[string]k8s.PodUtilization{}

	metricsCh := make(chan map[string]k8s.PodUtilization, 1)
	go pollPodMetricsForever(ctx, clients, ns, metricsCh)

	send := func() {
		out := render.Render(buildPodTable(columns, rows, metrics), render.Options{
			Total:       len(rows),
			Grep:        flags.Grep,
			SortColumn:  flags.SortColumn,
			ReverseSort: flags.SortColumn != "AGE",
			Limit:       displayLimit,
		})
		if err := sock.Send(bufferName, out); err != nil {
			log.Printf("[pods] send: %v", err)
		}
	}

	for ctx.Err() == nil {
		snap, err := clients.ListResource(ctx, id, ns)
		if err != nil {
			log.Printf("[pods] list: %v", err)
			sleep(ctx, relistBackoff)
			continue
		}

		columns = snap.Columns
		rows = make(map[string]k8s.Row, len(snap.Rows))
		for _, r := range snap.Rows {
			rows[rowKey(r)] = r
		}
		send()

		events, errs := clients.WatchResource(ctx, id, ns, snap.ResourceVersion)
		if !drainPodEvents(ctx, events, errs, metricsCh, rows, &metrics, send) {
			return
		}
	}
}

func drainPodEvents(
	ctx context.Context,
	events <-chan k8s.Event,
	errs <-chan error,
	metricsCh <-chan map[string]k8s.PodUtilization,
	rows map[string]k8s.Row,
	metrics *map[string]k8s.PodUtilization,
	send func(),
) bool {
	for {
		select {
		case <-ctx.Done():
			return false
		case ev, ok := <-events:
			if !ok {
				return true
			}
			k := rowKey(ev.Row)
			switch ev.Type {
			case "ADDED", "MODIFIED":
				rows[k] = ev.Row
			case "DELETED":
				delete(rows, k)
			}
			send()
		case err, ok := <-errs:
			if !ok {
				return true
			}
			if err != nil {
				log.Printf("[pods] watch: %v", err)
			}
			return true
		case m := <-metricsCh:
			*metrics = m
			send()
		}
	}
}

func pollPodMetricsForever(ctx context.Context, clients *k8s.Clients, ns string, out chan<- map[string]k8s.PodUtilization) {
	tick := time.NewTicker(metricsInterval)
	defer tick.Stop()

	fetch := func() {
		util, err := clients.FetchPodMetrics(ctx, ns)
		if err != nil {
			log.Printf("[pods] metrics: %v (continuing)", err)
			return
		}
		m := make(map[string]k8s.PodUtilization, len(util))
		for _, u := range util {
			m[u.Namespace+"/"+u.Name] = u
		}
		select {
		case out <- m:
		case <-ctx.Done():
		}
	}

	fetch()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
			fetch()
		}
	}
}

// rowKey extracts a stable identity from a row. We try the typed object's namespace/name
// first; if that's missing we fall back to the rendered NAME cell, prefixed nothing
// special since the apiserver guarantees uniqueness within (namespace, kind).
func rowKey(r k8s.Row) string {
	if pod, _ := k8s.DecodePod(r); pod != nil && pod.Name != "" {
		return pod.Namespace + "/" + pod.Name
	}
	// For non-pod rows we don't decode the typed object; use the NAME cell.
	if len(r.Cells) > 0 {
		return r.Cells[0]
	}
	return ""
}

func sleep(ctx context.Context, d time.Duration) {
	select {
	case <-time.After(d):
	case <-ctx.Done():
	}
}

// buildGenericTable: kind prefix on the NAME cell ("deployment.apps/foo"), rest of
// cells passthrough.
func buildGenericTable(snapCols []k8s.TableColumn, rowMap map[string]k8s.Row, plural string) render.Table {
	cols := make([]render.Column, len(snapCols))
	for i, c := range snapCols {
		cols[i] = render.Column{Name: c.Name, Format: c.Format}
	}

	nameIdx := nameColumnIndex(snapCols)
	prefix := kindPrefixFor(plural)

	rows := make([]render.Row, 0, len(rowMap))
	for _, r := range rowMap {
		cells := make([]string, len(r.Cells))
		copy(cells, r.Cells)
		if nameIdx >= 0 && nameIdx < len(cells) && prefix != "" {
			cells[nameIdx] = prefix + "/" + cells[nameIdx]
		}
		rows = append(rows, render.Row{Cells: cells})
	}
	return render.Table{Columns: cols, Rows: rows}
}

// buildPodTable: NAME first, then 6 metric cols, then everything else from server.
func buildPodTable(snapCols []k8s.TableColumn, rowMap map[string]k8s.Row, util map[string]k8s.PodUtilization) render.Table {
	metricCols := []render.Column{
		{Name: "CReq", Format: "percent-bar"},
		{Name: "CLim", Format: "percent-bar"},
		{Name: "CUse", Format: "percent-bar"},
		{Name: "MReq", Format: "percent-bar"},
		{Name: "MLim", Format: "percent-bar"},
		{Name: "MUse", Format: "percent-bar"},
	}

	nameIdx := nameColumnIndex(snapCols)

	cols := make([]render.Column, 0, len(metricCols)+len(snapCols))
	if nameIdx < 0 {
		for _, c := range snapCols {
			cols = append(cols, render.Column{Name: c.Name, Format: c.Format})
		}
	} else {
		cols = append(cols, render.Column{Name: "NAME", Format: snapCols[nameIdx].Format})
		cols = append(cols, metricCols...)
		for i, c := range snapCols {
			if i == nameIdx {
				continue
			}
			cols = append(cols, render.Column{Name: c.Name, Format: c.Format})
		}
	}

	rows := make([]render.Row, 0, len(rowMap))
	for _, r := range rowMap {
		pod, _ := k8s.DecodePod(r)
		if pod == nil {
			continue
		}

		creq, mreq := k8s.SumContainerRequests(pod)
		clim, mlim := k8s.SumContainerLimits(pod)

		var cuseStr, museStr string
		if u, ok := util[pod.Namespace+"/"+pod.Name]; ok {
			cuseStr = formatCPUUtil(u.CPUMilli, creq)
			museStr = formatMemUtil(u.MemMi, mreq)
		}

		var cells []string
		if nameIdx < 0 {
			cells = append([]string{}, r.Cells...)
		} else {
			cells = append(cells, "pod/"+pod.Name)
			cells = append(cells,
				formatCPU(creq), formatCPU(clim), cuseStr,
				formatMem(mreq), formatMem(mlim), museStr,
			)
			for i, c := range r.Cells {
				if i == nameIdx {
					continue
				}
				cells = append(cells, c)
			}
		}
		rows = append(rows, render.Row{Cells: cells})
	}
	return render.Table{Columns: cols, Rows: rows}
}

func nameColumnIndex(cols []k8s.TableColumn) int {
	for i, c := range cols {
		if c.Name == "NAME" || c.Name == "Name" {
			return i
		}
	}
	return -1
}

// kindPrefixFor returns the "kind.group" prefix to glue onto the NAME cell so output
// matches kubectl's `--show-kind=true` style.
func kindPrefixFor(plural string) string {
	switch plural {
	case "pods":
		return "pod"
	case "services":
		return "service"
	case "configmaps":
		return "configmap"
	case "deployments":
		return "deployment.apps"
	case "daemonsets":
		return "daemonset.apps"
	case "statefulsets":
		return "statefulset.apps"
	case "ingresses":
		return "ingress.networking.k8s.io"
	}
	return ""
}

func formatCPU(milli int64) string {
	if milli == 0 {
		return "0m"
	}
	return fmt.Sprintf("%dm", milli)
}

func formatMem(mi int64) string {
	if mi == 0 {
		return "0Mi"
	}
	return fmt.Sprintf("%dMi", mi)
}

func formatCPUUtil(useMilli, reqMilli int64) string {
	if reqMilli <= 0 || useMilli <= 0 {
		return ""
	}
	pct := int(useMilli * 100 / reqMilli)
	return fmt.Sprintf("%s %dm", render.PercentAsLines(pct), useMilli)
}

func formatMemUtil(useMi, reqMi int64) string {
	if reqMi <= 0 || useMi <= 0 {
		return ""
	}
	pct := int(useMi * 100 / reqMi)
	return fmt.Sprintf("%s %dMi", render.PercentAsLines(pct), useMi)
}

// runNodeLoop is the unconditional node watcher. Same pattern as the resource loops
// but with two side goroutines (metrics @60s, cluster-pod aggregation @15s) feeding
// the renderer, and node-specific column assembly.
func runNodeLoop(ctx context.Context, clients *k8s.Clients, sock *socket.Client, flags *Flags, bufferName string) {
	rows := map[string]k8s.Row{} // key: node name
	var columns []k8s.TableColumn
	metrics := map[string]k8s.NodeUtilization{}
	aggregates := map[string]*k8s.NodeAggregate{}

	metricsCh := make(chan map[string]k8s.NodeUtilization, 1)
	go pollNodeMetricsForever(ctx, clients, metricsCh)

	aggCh := make(chan map[string]*k8s.NodeAggregate, 1)
	go pollNodeAggregatesForever(ctx, clients, aggCh)

	send := func() {
		out := render.Render(buildNodeTable(columns, rows, metrics, aggregates), render.Options{
			Total:       len(rows),
			Grep:        flags.Grep,
			SortColumn:  flags.SortColumn,
			ReverseSort: flags.SortColumn != "AGE",
			Limit:       0, // show all nodes
		})
		if err := sock.Send(bufferName, out); err != nil {
			log.Printf("[nodes] send: %v", err)
		}
	}

	for ctx.Err() == nil {
		snap, err := clients.ListResource(ctx, k8s.NodeResource, "")
		if err != nil {
			log.Printf("[nodes] list: %v", err)
			sleep(ctx, relistBackoff)
			continue
		}

		columns = snap.Columns
		rows = make(map[string]k8s.Row, len(snap.Rows))
		for _, r := range snap.Rows {
			if name := nodeRowKey(r); name != "" {
				rows[name] = r
			}
		}
		send()

		events, errs := clients.WatchResource(ctx, k8s.NodeResource, "", snap.ResourceVersion)
		if !drainNodeEvents(ctx, events, errs, metricsCh, aggCh, rows, &metrics, &aggregates, send) {
			return
		}
	}
}

func drainNodeEvents(
	ctx context.Context,
	events <-chan k8s.Event,
	errs <-chan error,
	metricsCh <-chan map[string]k8s.NodeUtilization,
	aggCh <-chan map[string]*k8s.NodeAggregate,
	rows map[string]k8s.Row,
	metrics *map[string]k8s.NodeUtilization,
	aggregates *map[string]*k8s.NodeAggregate,
	send func(),
) bool {
	for {
		select {
		case <-ctx.Done():
			return false
		case ev, ok := <-events:
			if !ok {
				return true
			}
			k := nodeRowKey(ev.Row)
			if k == "" {
				continue
			}
			switch ev.Type {
			case "ADDED", "MODIFIED":
				rows[k] = ev.Row
			case "DELETED":
				delete(rows, k)
			}
			send()
		case err, ok := <-errs:
			if !ok {
				return true
			}
			if err != nil {
				log.Printf("[nodes] watch: %v", err)
			}
			return true
		case m := <-metricsCh:
			*metrics = m
			send()
		case a := <-aggCh:
			*aggregates = a
			send()
		}
	}
}

func pollNodeMetricsForever(ctx context.Context, clients *k8s.Clients, out chan<- map[string]k8s.NodeUtilization) {
	tick := time.NewTicker(nodeMetricsInterval)
	defer tick.Stop()

	fetch := func() {
		util, err := clients.FetchNodeMetrics(ctx)
		if err != nil {
			log.Printf("[nodes] metrics: %v (continuing)", err)
			return
		}
		m := make(map[string]k8s.NodeUtilization, len(util))
		for _, u := range util {
			m[u.Name] = u
		}
		select {
		case out <- m:
		case <-ctx.Done():
		}
	}

	fetch()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
			fetch()
		}
	}
}

func pollNodeAggregatesForever(ctx context.Context, clients *k8s.Clients, out chan<- map[string]*k8s.NodeAggregate) {
	tick := time.NewTicker(nodeAggregateInterval)
	defer tick.Stop()

	fetch := func() {
		agg, err := clients.FetchClusterPodAggregates(ctx)
		if err != nil {
			log.Printf("[nodes] cluster pod aggregate: %v (continuing)", err)
			return
		}
		select {
		case out <- agg:
		case <-ctx.Done():
		}
	}

	fetch()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
			fetch()
		}
	}
}

func nodeRowKey(r k8s.Row) string {
	if node, _ := k8s.DecodeNode(r); node != nil && node.Name != "" {
		return node.Name
	}
	if len(r.Cells) > 0 {
		return r.Cells[0]
	}
	return ""
}

// buildNodeTable composes the node row in the order python used:
//   NAME, Pods, CReq, CUse, MReq, MUse, GPUs, [server cols sans NAME], [label cols]
func buildNodeTable(
	snapCols []k8s.TableColumn,
	rowMap map[string]k8s.Row,
	metrics map[string]k8s.NodeUtilization,
	aggregates map[string]*k8s.NodeAggregate,
) render.Table {
	customCols := []render.Column{
		{Name: "Pods"},
		{Name: "CReq", Format: "percent-bar"},
		{Name: "CUse", Format: "percent-bar"},
		{Name: "MReq", Format: "percent-bar"},
		{Name: "MUse", Format: "percent-bar"},
		{Name: "GPUs"},
	}

	nameIdx := nameColumnIndex(snapCols)
	cols := make([]render.Column, 0, 1+len(customCols)+len(snapCols)+len(nodeLabelColumns))

	if nameIdx < 0 {
		// Server didn't give us a NAME column; use whatever it gave plus our cols.
		for _, c := range snapCols {
			cols = append(cols, render.Column{Name: c.Name, Format: c.Format})
		}
		cols = append(cols, customCols...)
	} else {
		cols = append(cols, render.Column{Name: "NAME", Format: snapCols[nameIdx].Format})
		cols = append(cols, customCols...)
		for i, c := range snapCols {
			if i == nameIdx {
				continue
			}
			cols = append(cols, render.Column{Name: c.Name, Format: c.Format})
		}
	}
	for _, lc := range nodeLabelColumns {
		cols = append(cols, render.Column{Name: lc.Header})
	}

	rows := make([]render.Row, 0, len(rowMap))
	for _, r := range rowMap {
		node, _ := k8s.DecodeNode(r)
		if node == nil {
			continue
		}
		// Skip Fargate nodes — same as python.
		if strings.Contains(node.Name, "fargate") {
			continue
		}

		allocCPU, allocMem := k8s.NodeAllocatable(node)
		agg := aggregates[node.Name]
		met, hasMet := metrics[node.Name]

		podsStr := ""
		gpusStr := "."
		var creqStr, mreqStr string
		if agg != nil {
			podsStr = fmt.Sprintf("%d", agg.PodCount)
			gpusStr = fmt.Sprintf("%d", agg.GPUCount)
			creqStr = formatNodeCPUMetric(agg.CPUMilli, allocCPU)
			mreqStr = formatNodeMemMetric(agg.MemMi, allocMem)
		}
		var cuseStr, museStr string
		if hasMet {
			cuseStr = formatNodeCPUMetric(met.CPUMilli, allocCPU)
			museStr = formatNodeMemMetric(met.MemMi, allocMem)
		}

		var cells []string
		if nameIdx < 0 {
			cells = append([]string{}, r.Cells...)
			cells = append(cells, podsStr, creqStr, cuseStr, mreqStr, museStr, gpusStr)
		} else {
			cells = append(cells, "node/"+node.Name)
			cells = append(cells, podsStr, creqStr, cuseStr, mreqStr, museStr, gpusStr)
			for i, c := range r.Cells {
				if i == nameIdx {
					continue
				}
				cells = append(cells, c)
			}
		}
		// Label columns at the end.
		for _, lc := range nodeLabelColumns {
			cells = append(cells, node.Labels[lc.Key])
		}
		rows = append(rows, render.Row{Cells: cells})
	}
	return render.Table{Columns: cols, Rows: rows}
}

// formatNodeCPUMetric: "{bars} {N}" where N is rounded CPU cores (millicores/1000).
// Mirrors python's make_metric_pretty + round_cpu.
func formatNodeCPUMetric(useMilli, allocMilli int64) string {
	if allocMilli <= 0 || useMilli <= 0 {
		return ""
	}
	pct := int(useMilli * 100 / allocMilli)
	cores := float64(useMilli) / 1000.0
	return fmt.Sprintf("%s %.1f", render.PercentAsLines(pct), cores)
}

// formatNodeMemMetric: "{bars} {N}Gi", with correct binary-unit conversion (1 Gi = 1024 Mi).
func formatNodeMemMetric(useMi, allocMi int64) string {
	if allocMi <= 0 || useMi <= 0 {
		return ""
	}
	pct := int(useMi * 100 / allocMi)
	gi := float64(useMi) / 1024.0
	return fmt.Sprintf("%s %.1fGi", render.PercentAsLines(pct), gi)
}

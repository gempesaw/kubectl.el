package k8s

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"strings"

	corev1 "k8s.io/api/core/v1"
	"k8s.io/client-go/rest"
)

// ResourceID identifies a Kubernetes resource we know how to watch via the Table API.
type ResourceID struct {
	Alias      string // user-facing alias as passed in argv ("po", "ds", "deploy", ...)
	Plural     string // apiserver resource name ("pods", "daemonsets", ...)
	Group      string // "" for core, "apps" / "networking.k8s.io" / etc.
	Version    string // typically "v1"
	Namespaced bool
}

type Snapshot struct {
	Columns         []TableColumn
	Rows            []Row
	ResourceVersion string
}

type TableColumn struct {
	Name   string
	Format string
}

// Row carries server-rendered cells plus the raw object JSON, for callers that need
// typed access (e.g. pods reading container resources off the spec).
type Row struct {
	Cells  []string
	Object json.RawMessage
}

type Event struct {
	Type string // "ADDED" | "MODIFIED" | "DELETED"
	Row  Row
}

type jsonTable struct {
	Metadata struct {
		ResourceVersion string `json:"resourceVersion"`
	} `json:"metadata"`
	ColumnDefinitions []struct {
		Name   string `json:"name"`
		Type   string `json:"type"`
		Format string `json:"format"`
	} `json:"columnDefinitions"`
	Rows []jsonTableRow `json:"rows"`
}

type jsonTableRow struct {
	Cells  []json.RawMessage `json:"cells"`
	Object json.RawMessage   `json:"object"`
}

// ListResource fetches the server-rendered Table for the given resource.
func (c *Clients) ListResource(ctx context.Context, id ResourceID, namespace string) (*Snapshot, error) {
	raw, err := c.resourceRequest(id, namespace).DoRaw(ctx)
	if err != nil {
		return nil, fmt.Errorf("list %s: %w", id.Plural, err)
	}

	var t jsonTable
	if err := json.Unmarshal(raw, &t); err != nil {
		return nil, fmt.Errorf("decode %s table: %w", id.Plural, err)
	}

	cols := make([]TableColumn, len(t.ColumnDefinitions))
	for i, c := range t.ColumnDefinitions {
		cols[i] = TableColumn{Name: strings.ToUpper(c.Name), Format: c.Format}
	}

	rows := make([]Row, 0, len(t.Rows))
	for _, r := range t.Rows {
		rows = append(rows, decodeRow(r))
	}

	return &Snapshot{Columns: cols, Rows: rows, ResourceVersion: t.Metadata.ResourceVersion}, nil
}

// WatchResource opens a streaming watch from the given resourceVersion. Channels close
// when the watch ends; caller is expected to re-list and resume.
func (c *Clients) WatchResource(ctx context.Context, id ResourceID, namespace, resourceVersion string) (<-chan Event, <-chan error) {
	events := make(chan Event)
	errs := make(chan error, 1)

	go func() {
		defer close(events)
		defer close(errs)

		req := c.resourceRequest(id, namespace).
			Param("watch", "true").
			Param("resourceVersion", resourceVersion).
			Param("allowWatchBookmarks", "true")

		stream, err := req.Stream(ctx)
		if err != nil {
			errs <- fmt.Errorf("watch %s: %w", id.Plural, err)
			return
		}
		defer stream.Close()

		decoder := json.NewDecoder(stream)
		for {
			var raw struct {
				Type   string          `json:"type"`
				Object json.RawMessage `json:"object"`
			}
			if err := decoder.Decode(&raw); err != nil {
				if err != io.EOF && ctx.Err() == nil {
					errs <- fmt.Errorf("decode %s event: %w", id.Plural, err)
				}
				return
			}

			if raw.Type != "ADDED" && raw.Type != "MODIFIED" && raw.Type != "DELETED" {
				continue
			}

			var t jsonTable
			if err := json.Unmarshal(raw.Object, &t); err != nil {
				continue
			}
			if len(t.Rows) == 0 {
				continue
			}

			select {
			case events <- Event{Type: raw.Type, Row: decodeRow(t.Rows[0])}:
			case <-ctx.Done():
				return
			}
		}
	}()

	return events, errs
}

func (c *Clients) resourceRequest(id ResourceID, namespace string) *rest.Request {
	base := "/api/v1"
	if id.Group != "" {
		base = fmt.Sprintf("/apis/%s/%s", id.Group, id.Version)
	}

	var path string
	if id.Namespaced && namespace != "" {
		path = fmt.Sprintf("%s/namespaces/%s/%s", base, namespace, id.Plural)
	} else {
		path = fmt.Sprintf("%s/%s", base, id.Plural)
	}

	return c.Core.CoreV1().RESTClient().Get().AbsPath(path).
		SetHeader("Accept", "application/json;as=Table;v=v1;g=meta.k8s.io,application/json").
		Param("includeObject", "Object")
}

func decodeRow(r jsonTableRow) Row {
	cells := make([]string, len(r.Cells))
	for i, raw := range r.Cells {
		cells[i] = cellToString(raw)
	}
	return Row{Cells: cells, Object: r.Object}
}

// cellToString renders a Table cell — string, number, or bool — into a display string.
func cellToString(raw json.RawMessage) string {
	if len(raw) == 0 {
		return ""
	}
	var s string
	if err := json.Unmarshal(raw, &s); err == nil {
		return s
	}
	var n json.Number
	if err := json.Unmarshal(raw, &n); err == nil {
		return n.String()
	}
	var b bool
	if err := json.Unmarshal(raw, &b); err == nil {
		if b {
			return "true"
		}
		return "false"
	}
	return string(raw)
}

// DecodePod unpacks a Row's Object as a *corev1.Pod. Returns (nil, nil) if the row
// has no object payload.
func DecodePod(r Row) (*corev1.Pod, error) {
	if len(r.Object) == 0 {
		return nil, nil
	}
	var pod corev1.Pod
	if err := json.Unmarshal(r.Object, &pod); err != nil {
		return nil, err
	}
	return &pod, nil
}

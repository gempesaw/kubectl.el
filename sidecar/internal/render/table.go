package render

import (
	"fmt"
	"sort"
	"strings"
)

// Column describes a column in a rendered table. Type is borrowed from
// metav1.TableColumnDefinition.Format ("" string default, "name", "date" for AGE-like, etc.)
// but we only really care about a few hints for sorting.
type Column struct {
	Name   string
	Format string // "date" for AGE; "" for plain string; "percent-bar" for our metric cols
}

// Row is a list of cell strings, one per column in the surrounding Table.
type Row struct {
	Cells []string
}

type Table struct {
	Columns []Column
	Rows    []Row
}

// Options drive a single render pass.
type Options struct {
	Total       int    // total count regardless of grep — for the "NAME N" header trick
	Grep        string // empty or "-" disables grep
	SortColumn  string // column name to sort by; falls back to NAME
	ReverseSort bool
	Limit       int // max rows in output
}

// Render produces a kubectl-style left-aligned, 2-space-padded table string.
// The first row's NAME column is suffixed with the total count after the column name.
func Render(t Table, opts Options) string {
	rows := t.Rows
	if opts.Grep != "" && opts.Grep != "-" {
		rows = filterRows(rows, opts.Grep)
	}

	// Nothing to show — don't even emit a header. The dashboard's join logic skips
	// empty sections, so this is how we "hide" a resource that has zero matches.
	if len(rows) == 0 {
		return ""
	}

	sortIdx := columnIndex(t.Columns, opts.SortColumn)
	if sortIdx < 0 {
		sortIdx = columnIndex(t.Columns, "NAME")
	}
	if sortIdx >= 0 {
		sortRows(rows, t.Columns[sortIdx], sortIdx, opts.ReverseSort)
	}

	if opts.Limit > 0 && len(rows) > opts.Limit {
		rows = rows[:opts.Limit]
	}

	headers := make([]string, len(t.Columns))
	for i, c := range t.Columns {
		headers[i] = c.Name
	}
	if nameIdx := columnIndex(t.Columns, "NAME"); nameIdx >= 0 {
		headers[nameIdx] = fmt.Sprintf("NAME %d", opts.Total)
	}

	return formatTable(headers, rows)
}

func filterRows(rows []Row, needle string) []Row {
	out := make([]Row, 0, len(rows))
	for _, r := range rows {
		joined := strings.Join(r.Cells, " ")
		if strings.Contains(joined, needle) {
			out = append(out, r)
		}
	}
	return out
}

func columnIndex(cols []Column, name string) int {
	for i, c := range cols {
		if c.Name == name {
			return i
		}
	}
	return -1
}

func sortRows(rows []Row, col Column, idx int, reverse bool) {
	cmp := comparatorFor(col)
	sort.SliceStable(rows, func(i, j int) bool {
		ai, aj := "", ""
		if idx < len(rows[i].Cells) {
			ai = rows[i].Cells[idx]
		}
		if idx < len(rows[j].Cells) {
			aj = rows[j].Cells[idx]
		}
		less := cmp(ai, aj)
		if reverse {
			return !less
		}
		return less
	})
}

func formatTable(headers []string, rows []Row) string {
	cols := len(headers)
	widths := make([]int, cols)
	for i, h := range headers {
		widths[i] = len(h)
	}
	for _, r := range rows {
		for i := 0; i < cols && i < len(r.Cells); i++ {
			if l := len(r.Cells[i]); l > widths[i] {
				widths[i] = l
			}
		}
	}

	var b strings.Builder
	writeRow(&b, headers, widths)
	for _, r := range rows {
		writeRow(&b, r.Cells, widths)
	}
	return b.String()
}

func writeRow(b *strings.Builder, cells []string, widths []int) {
	for i, w := range widths {
		var cell string
		if i < len(cells) {
			cell = cells[i]
		}
		// Left-align with 2-space right padding (matches PrettyTable PLAIN_COLUMNS look).
		b.WriteString(cell)
		pad := w - len(cell) + 2
		for k := 0; k < pad; k++ {
			b.WriteByte(' ')
		}
	}
	b.WriteByte('\n')
}

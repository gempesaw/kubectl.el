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
		sortRows(rows, t.Columns[sortIdx], sortIdx, columnIndex(t.Columns, "NAME"), opts.ReverseSort)
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

// sortRows sorts ROWS in place by the primary column COL (at IDX), then by NAME
// (at NAMEIDX) as a tie-breaker so the output is stable across renders. NAME
// tie-break always ascends, even when the primary is reversed — flipping the
// tie-break with reverse would just shuffle equal-primary groups in opposite
// directions on each render of a "reversed AGE" column where many rows share
// the same minute-granularity value.
func sortRows(rows []Row, col Column, idx, nameIdx int, reverse bool) {
	primary := comparatorFor(col)
	sort.SliceStable(rows, func(i, j int) bool {
		ai, aj := cellAt(rows[i], idx), cellAt(rows[j], idx)
		aLess := primary(ai, aj)
		bLess := primary(aj, ai)
		if aLess != bLess {
			if reverse {
				return bLess
			}
			return aLess
		}
		// Tie on primary — fall back to NAME ascending unless that's the
		// primary column already (in which case nothing more to do).
		if nameIdx < 0 || nameIdx == idx {
			return false
		}
		ni, nj := cellAt(rows[i], nameIdx), cellAt(rows[j], nameIdx)
		return naturalLess(ni, nj)
	})
}

func cellAt(r Row, idx int) string {
	if idx < 0 || idx >= len(r.Cells) {
		return ""
	}
	return r.Cells[idx]
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

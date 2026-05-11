package render

import "math"

// PercentAsLines mirrors watch.py's percent_as_lines: scales 0-100% to 1-6 vertical bars.
// 0% rounds up to 1 bar (matches python: util==0 -> util=1).
func PercentAsLines(percent int) string {
	if percent <= 0 {
		percent = 1
	}
	bars := int(math.Ceil(float64(percent) / 20.0))
	if bars > 6 {
		bars = 6
	}
	out := make([]byte, bars)
	for i := range out {
		out[i] = '|'
	}
	return string(out)
}

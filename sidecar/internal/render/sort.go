package render

import (
	"regexp"
	"strconv"
	"strings"
)

type comparator func(a, b string) bool

func comparatorFor(col Column) comparator {
	switch col.Format {
	case "date":
		// "date" in kubectl's Table API == AGE-style relative duration ("3h", "5d12h").
		return ageLess
	case "percent-bar":
		return percentLeadingNumberLess
	}
	switch col.Name {
	case "AGE":
		return ageLess
	case "CReq", "CLim", "CUse", "MReq", "MLim", "MUse":
		return percentLeadingNumberLess
	}
	return stringLess
}

func stringLess(a, b string) bool { return a < b }

var ageRe = regexp.MustCompile(`(?:(\d+)d)?(?:(\d+)h)?(?:(\d+)m)?(?:(\d+)s)?`)

func ageLess(a, b string) bool {
	return ageSeconds(a) < ageSeconds(b)
}

func ageSeconds(s string) int {
	m := ageRe.FindStringSubmatch(s)
	if m == nil {
		return 0
	}
	d, _ := strconv.Atoi(m[1])
	h, _ := strconv.Atoi(m[2])
	mins, _ := strconv.Atoi(m[3])
	secs, _ := strconv.Atoi(m[4])
	return d*86400 + h*3600 + mins*60 + secs
}

var leadingNumRe = regexp.MustCompile(`\d+`)

// percentLeadingNumberLess pulls the first integer it can find. Mirrors
// watch.py's sort_with_percent: split on space, strip non-digits, parse int.
func percentLeadingNumberLess(a, b string) bool {
	return leadingNumber(a) < leadingNumber(b)
}

func leadingNumber(s string) int {
	parts := strings.SplitN(s, " ", 2)
	m := leadingNumRe.FindString(parts[0])
	if m == "" {
		return 0
	}
	n, _ := strconv.Atoi(m)
	return n
}

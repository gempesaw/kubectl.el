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
	return naturalLess
}

// naturalLess compares strings so embedded numbers sort numerically:
//   "node-9" < "node-10" (lex would put "node-10" first because '1' < '9')
//   "00", "01", "02"     still sort correctly (zero-padded case works either way)
// Walks both strings simultaneously, treating digit runs as a single integer
// comparison and everything else as char-by-char.
func naturalLess(a, b string) bool {
	i, j := 0, 0
	for i < len(a) && j < len(b) {
		ai, bi := a[i], b[j]
		aDigit := ai >= '0' && ai <= '9'
		bDigit := bi >= '0' && bi <= '9'
		if aDigit && bDigit {
			aEnd := i
			for aEnd < len(a) && a[aEnd] >= '0' && a[aEnd] <= '9' {
				aEnd++
			}
			bEnd := j
			for bEnd < len(b) && b[bEnd] >= '0' && b[bEnd] <= '9' {
				bEnd++
			}
			// Strip leading zeros for comparison; shorter run = smaller number.
			as := strings.TrimLeft(a[i:aEnd], "0")
			bs := strings.TrimLeft(b[j:bEnd], "0")
			if len(as) != len(bs) {
				return len(as) < len(bs)
			}
			if as != bs {
				return as < bs
			}
			// Numeric values equal; tie-break by original run length (more
			// leading zeros = "smaller" so "01" < "1").
			if aEnd-i != bEnd-j {
				return aEnd-i > bEnd-j
			}
			i, j = aEnd, bEnd
			continue
		}
		if ai != bi {
			return ai < bi
		}
		i++
		j++
	}
	return len(a) < len(b)
}

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

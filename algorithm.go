// package bidi implements an algorithm or something
// TODO(lutzky): Fix description
package bidi

//go:generate go run gen-bidirectional.go

import (
	"fmt"
	"log"
	"os"
	"runtime"
	"strconv"
	"unicode"
)

var PARAGRAPH_LEVELS = map[string]int{"L": 0, "AL": 1, "R": 1}

const EXPLICIT_LEVEL_LIMIT = 62

func _LEAST_GREATER_ODD(x int) int {
	return (x + 1) | 1
}

func _LEAST_GREATER_EVEN(x int) int {
	return (x + 2) & ^1
}

var X2_X5_MAPPINGS = map[string]struct {
	f func(int) int
	s string
}{
	"RLE": {_LEAST_GREATER_ODD, "N"},
	"LRE": {_LEAST_GREATER_EVEN, "N"},
	"RLO": {_LEAST_GREATER_ODD, "R"},
	"LRO": {_LEAST_GREATER_EVEN, "L"},
}

type run struct {
	sor, eor string
	start    int
	length   int
	Type     string
}

type Char struct {
	ch    rune
	level int
	Type  string
	orig  string
}

type Storage struct {
	base_level int
	base_dir   string
	chars      []Char
	runs       []run
}

// Added 'B' so X6 won't execute in that case and X8 will run it's course
var X6_IGNORED = X2_X5_MAPPINGS_keys().plus("BN", "PDF", "B")
var X9_REMOVED = X2_X5_MAPPINGS_keys().plus("BN", "PDF")

func X2_X5_MAPPINGS_keys() stringSet {
	result := stringSet{}
	for k := range X2_X5_MAPPINGS {
		result[k] = true
	}
	return result
}

// TODO(lutzky): Start stringSet stuff
type stringSet map[string]bool

func newStringSet(s ...string) stringSet {
	result := stringSet{}
	for _, x := range s {
		result[x] = true
	}
	return result
}

func (ss stringSet) plus(s ...string) stringSet {
	result := newStringSet(s...)
	for x := range ss {
		result[x] = true
	}
	return result
}

// TODO(lutzky): End stringSet stuff

func _embedding_direction(x int) string {
	return []string{"L", "R"}[x%2]
}

// _IS_UCS2 = sys.maxunicode == 65535
const _IS_UCS2 = true
const _SURROGATE_MIN = 0xd800
const _SURROGATE_MAX = 0xdbff

type debug_params struct {
	base_info bool
	no_chars  bool
	runs      bool
}

// Display debug information for the storage
func debug_storage(storage *Storage, params debug_params) {
	l := log.New(os.Stderr, "", 0)

	pc := make([]uintptr, 1)
	n := runtime.Callers(2, pc)
	if n == 0 {
		panic("Failed to get name of calling function")
	}
	frames := runtime.CallersFrames(pc)
	frame, _ := frames.Next()

	l.Printf("in %s\n", frame.Function)

	if params.base_info {
		l.Printf("  base level  : %d", storage.base_level)
		l.Printf("  base dir    : %s", storage.base_dir)
	}

	if params.runs {
		l.Printf("  runs        : %+v", storage.runs)
	}

	if !params.no_chars {
		output := "  Chars       : "
		for _, _ch := range storage.chars {
			output += string(_ch.ch)
		}
		l.Printf(output + "\n")

		output = "  Res. levels : "
		for _, _ch := range storage.chars {
			output += strconv.Itoa(_ch.level)
		}
		l.Printf(output)

		_types := make([]string, len(storage.chars))
		for i, _ch := range storage.chars {
			_types[i] = fmt.Sprintf("%-3s", _ch.Type)
		}

		for i := 0; i < 3; i++ {
			if i != 0 {
				output = "                %s\n"
			} else {
				output = "  Res. types  : %s\n"
			}
			extra_output := ""
			for _, _t := range _types {
				extra_output += string(_t[i])
			}
			l.Printf(output, extra_output)
		}
	}

}

func bidirectional(_ch rune) string {
	return Bidirectional[_ch]
}

/*
Get the paragraph base embedding level. Returns 0 for LTR,
1 for RTL.

`text` a unicode object.

Set `upper_is_rtl` to true to treat upper case chars as strong "R"
for debugging (default: false).
*/
func get_base_level(text []rune, upper_is_rtl bool) int {
	base_level := -1
	var prev_surrogate rune

	// P2
	for i, _ch := range text {
		// surrogate in case of ucs2
		if _IS_UCS2 && (_SURROGATE_MIN <= _ch) && (_ch <= _SURROGATE_MAX) {
			prev_surrogate = _ch
		} else if prev_surrogate != 0 {
			text[i] += prev_surrogate
			prev_surrogate = 0
		}

		// treat upper as RTL ?
		if upper_is_rtl && unicode.IsUpper(_ch) {
			base_level = 1
			break
		}

		bidi_type := bidirectional(_ch)

		if bidi_type == "AL" || bidi_type == "R" {
			base_level = 1
			break
		} else if bidi_type == "L" {
			base_level = 0
			break
		}
	}

	// P3
	if base_level == -1 {
		base_level = 0
	}

	return base_level
}

// Get the paragraph base embedding level and direction,
// set the storage to the array of chars
func get_embedding_levels(text []rune, storage *Storage, upper_is_rtl bool, debug bool) {
	var prev_surrogate rune
	base_level := storage.base_level
	var bidi_type string

	// preset the storage's chars
	for i, _ch := range text {
		if _IS_UCS2 && (_SURROGATE_MIN <= _ch) && (_ch <= _SURROGATE_MAX) {
			prev_surrogate = _ch
			continue
		} else if prev_surrogate != 0 {
			text[i] += prev_surrogate
			prev_surrogate = 0
		}

		if upper_is_rtl && unicode.IsUpper(_ch) {
			bidi_type = "R"
		} else {
			bidi_type = bidirectional(_ch)
		}

		storage.chars = append(storage.chars, Char{
			ch:    _ch,
			level: base_level,
			Type:  bidi_type,
			orig:  bidi_type,
		})
	}
	if debug {
		debug_storage(storage, debug_params{base_info: true})
	}
}

type levelsType struct {
	embedding_level      int
	directional_override string
}

// Apply X1 to X9 rules of the unicode algorithm.
//
// See http://unicode.org/reports/tr9/#Explicit_Levels_and_Directions
func explicit_embed_and_overrides(storage *Storage, debug bool) {
	overflow_counter := 0
	almost_overflow_counter := 0
	directional_override := "N"
	var levels []levelsType

	// X1
	embedding_level := storage.base_level

	for i, _ch := range storage.chars {
		bidi_type := _ch.Type

		level_func := X2_X5_MAPPINGS[bidi_type].f
		override := X2_X5_MAPPINGS[bidi_type].s

		if level_func != nil {
			// So this is X2 to X5
			// if we've past EXPLICIT_LEVEL_LIMIT, note it and do nothing

			if overflow_counter != 0 {
				overflow_counter++
				continue
			}

			new_level := level_func(embedding_level)
			if new_level < EXPLICIT_LEVEL_LIMIT {
				levels = append(levels, levelsType{embedding_level, directional_override})
				embedding_level, directional_override = new_level, override
			} else if embedding_level == EXPLICIT_LEVEL_LIMIT-2 {
				// The new level is invalid, but a valid level can still be
				// achieved if this level is 60 and we encounter an RLE or
				// RLO further on.  So record that we 'almost' overflowed.
				almost_overflow_counter++
			} else {
				overflow_counter++
			}
		} else {
			// X6
			if !X6_IGNORED[bidi_type] {
				storage.chars[i].level = embedding_level
				if directional_override != "N" {
					storage.chars[i].Type = directional_override
				}
			} else if bidi_type == "PDF" {
				// X7
				if overflow_counter != 0 {
					overflow_counter--
				} else if almost_overflow_counter != 0 && embedding_level != EXPLICIT_LEVEL_LIMIT-1 {
					almost_overflow_counter -= 1
				} else if len(levels) > 0 {
					lTmp := levels[len(levels)-1]
					embedding_level = lTmp.embedding_level
					directional_override = lTmp.directional_override
					levels = levels[:len(levels)-1]
				}
			} else if bidi_type == "B" {
				// X8
				levels = nil
				overflow_counter = 0
				almost_overflow_counter = 0
				storage.chars[i].level = storage.base_level
				embedding_level = _ch.level
				directional_override = "N"
			}
		}
	}

	// Removes the explicit embeds and overrides of types
	// RLE, LRE, RLO, LRO, PDF, and BN. Adjusts extended chars
	// next and prev as well

	// Applies X9. See http://unicode.org/reports/tr9/#X9
	{
		tmp := storage.chars
		storage.chars = make([]Char, 0, len(tmp))
		for _, _ch := range tmp {
			if !X9_REMOVED[_ch.Type] {
				storage.chars = append(storage.chars, _ch)
			}
		}
	}

	calc_level_runs(storage)

	if debug {
		debug_storage(storage, debug_params{runs: true})
	}
}

// Split the storage to run of char types at the same level.
//
// Applies X10. See http://unicode.org/reports/tr9/#X10
func calc_level_runs(storage *Storage) {
	// run level depends on the higher of the two levels on either side of
	// the boundary If the higher level is odd, the type is R; otherwise,
	// it is L

	storage.runs = nil
	chars := storage.chars

	// empty string ?
	if len(chars) == 0 {
		return
	}

	calc_level_run := func(b_l int, b_r int) string {
		max := b_l
		if b_r > max {
			max = b_r
		}
		return []string{"L", "R"}[max%2]
	}

	first_char := chars[0]

	sor := calc_level_run(storage.base_level, first_char.level)
	eor := ""

	run_start := 0
	run_length := 0

	prev_level, prev_type := first_char.level, first_char.Type

	var (
		curr_level int
		curr_type  string
	)

	for _, _ch := range chars {
		curr_level, curr_type = _ch.level, _ch.Type

		if curr_level == prev_level {
			run_length++
		} else {
			eor = calc_level_run(prev_level, curr_level)
			storage.runs = append(storage.runs, run{sor: sor, eor: eor, start: run_start,
				Type: prev_type, length: run_length})
			sor = eor
			run_start += run_length
			run_length = 1
		}

		prev_level, prev_type = curr_level, curr_type
	}

	// for the last char/runlevel
	eor = calc_level_run(curr_level, storage.base_level)
	storage.runs = append(storage.runs, run{sor: sor, eor: eor, start: run_start,
		Type: curr_type, length: run_length})
}

// Resolve weak type rules W1 - W3.
//
// See: http://unicode.org/reports/tr9/#Resolving_Weak_Types
func resolve_weak_types(storage *Storage, debug bool) {
	for _, run := range storage.runs {
		prev_strong := run.sor
		prev_type := run.sor
		start, length := run.start, run.length
		chars := storage.chars[start : start+length]
		for i, _ch := range chars {
			// W1. Examine each nonspacing mark (NSM) in the level run, and
			// change the type of the NSM to the type of the previous character.
			// If the NSM is at the start of the level run, it will get the type
			// of sor.
			bidi_type := _ch.Type

			if bidi_type == "NSM" {
				bidi_type = prev_type
				chars[i].Type = bidi_type
			}

			// W2. Search backward from each instance of a European number until
			// the first strong type (R, L, AL, or sor) is found. If an AL is
			// found, change the type of the European number to Arabic number.
			if bidi_type == "EN" && prev_strong == "AL" {
				chars[i].Type = "AL"
			}

			// update prev_strong if needed
			if newStringSet("R", "L", "AL")[bidi_type] {
				prev_strong = bidi_type
			}

			prev_type = _ch.Type
		}

		// W3. Change all ALs to R
		for i, _ch := range chars {
			if _ch.Type == "AL" {
				chars[i].Type = "R"
			}
		}

		// W4. A single European separator between two European numbers changes
		// to a European number. A single common separator between two numbers of
		// the same type changes to that type.
		for idx := 1; idx < len(chars)-1; idx++ {
			bidi_type := chars[idx].Type
			prev_type := chars[idx-1].Type
			next_type := chars[idx+1].Type

			if bidi_type == "ES" && prev_type == "EN" && next_type == "EN" {
				chars[idx].Type = "EN"
			}

			if bidi_type == "CS" && prev_type == next_type &&
				(prev_type == "AL" || prev_type == "EN") {
				chars[idx].Type = prev_type
			}
		}

		// W5. A sequence of European terminators adjacent to European numbers
		// changes to all European numbers.
		for idx := range chars {
			if chars[idx].Type == "EN" {
				for et_idx := idx - 1; et_idx > -1; et_idx-- {
					if chars[et_idx].Type == "ET" {
						chars[et_idx].Type = "EN"
					} else {
						break
					}
				}
				for et_idx := idx + 1; et_idx < len(chars); et_idx++ {
					if chars[et_idx].Type == "ET" {
						chars[et_idx].Type = "EN"
					} else {
						break
					}
				}
			}
		}

		// W6. Otherwise, separators and terminators change to Other Neutral.
		for i, _ch := range chars {
			if newStringSet("ET", "ES", "CS")[_ch.Type] {
				chars[i].Type = "ON"
			}
		}

		// W7. Search backward from each instance of a European number until the
		// first strong type (R, L, or sor) is found. If an L is found, then
		// change the type of the European number to L.
		prev_strong = run.sor
		for i, _ch := range chars {
			if _ch.Type == "EN" && prev_strong == "L" {
				chars[i].Type = "L"
			}

			if _ch.Type == "L" || _ch.Type == "R" {
				prev_strong = _ch.Type
			}
		}
	}

	if debug {
		debug_storage(storage, debug_params{runs: true})
	}
}

// Resolving neutral types. Implements N1 and N2
//
// See: http://unicode.org/reports/tr9/#Resolving_Neutral_Types
func resolve_neutral_types(storage *Storage, debug bool) {
	for _, run := range storage.runs {
		start := run.start
		length := run.length
		// use sor and eor
		chars := []Char{{Type: run.sor}}
		for _, ch := range storage.chars[start : start+length] {
			chars = append(chars, ch)
		}
		chars = append(chars, Char{Type: run.eor})
		total_chars := len(chars)

		var prev_bidi_type, next_bidi_type string

		seq_start := -1
		for idx := 0; idx < total_chars; idx++ {
			_ch := chars[idx]
			if newStringSet("B", "S", "WS", "ON")[_ch.Type] {
				// N1. A sequence of neutrals takes the direction of the
				// surrounding strong text if the text on both sides has the same
				// direction. European and Arabic numbers act as if they were R
				// in terms of their influence on neutrals. Start-of-level-run
				// (sor) and end-of-level-run (eor) are used at level run
				// boundaries.
				if seq_start == -1 {
					seq_start = idx
					prev_bidi_type = chars[idx-1].Type
				}
			} else {
				if seq_start != -1 {
					next_bidi_type = chars[idx].Type

					if prev_bidi_type == "AN" || prev_bidi_type == "EN" {
						prev_bidi_type = "R"
					}

					if next_bidi_type == "AN" || next_bidi_type == "EN" {
						next_bidi_type = "R"
					}

					for seq_idx := seq_start; seq_idx < idx; seq_idx++ {
						if prev_bidi_type == next_bidi_type {
							storage.chars[start+seq_idx-1].Type = prev_bidi_type
						} else {
							// N2. Any remaining neutrals take the embedding
							// direction. The embedding direction for the given
							// neutral character is derived from its embedding
							// level: L if the character is set to an even level,
							// and R if the level is odd.
							storage.chars[start+seq_idx-1].Type =
								_embedding_direction(chars[seq_idx].level)
						}
					}

					seq_start = -1
				}
			}
		}
	}

	if debug {
		debug_storage(storage, debug_params{})
	}
}

// Resolving implicit levels (I1, I2)
//
// See: http://unicode.org/reports/tr9/#Resolving_Implicit_Levels
func resolve_implicit_levels(storage *Storage, debug bool) {
	for _, run := range storage.runs {
		start, length := run.start, run.length
		chars := storage.chars[start : start+length]

		for i, _ch := range chars {
			// only those types are allowed at this stage
			if !newStringSet("L", "R", "EN", "AL")[_ch.Type] {
				panic(_ch.Type + " not allowed here")
			}

			if _embedding_direction(_ch.level) == "L" {
				// I1. For all characters with an even (left-to-right) embedding
				// direction, those of type R go up one level and those of type
				// AN or EN go up two levels.
				if _ch.Type == "R" {
					chars[i].level += 1
				} else if _ch.Type != "L" {
					chars[i].level += 2
				}
			} else {
				// I2. For all characters with an odd (right-to-left) embedding
				// direction, those of type L, EN or AN  go up one level.
				if _ch.Type != "R" {
					chars[i].level += 1
				}
			}
		}
	}

	if debug {
		debug_storage(storage, debug_params{runs: true})
	}
}

func reverse(x []Char) {
	for i := 0; i < len(x)/2; i++ {
		opp := len(x) - i - 1
		x[i], x[opp] = x[opp], x[i]
	}
}

// L2. From the highest level found in the text to the lowest odd
// level on each line, including intermediate levels not actually
// present in the text, reverse any contiguous sequence of characters
// that are at that level or higher.
func reverse_contiguous_sequence(chars []Char, line_start, line_end, highest_level,
	lowest_odd_level int) {
	for level := highest_level; level > lowest_odd_level-1; level-- {
		_start := -1
		_end := -1

		for run_idx := line_start; run_idx < line_end+1; run_idx++ {
			run_ch := chars[run_idx]

			if run_ch.level >= level {
				if _start == -1 {
					_start = run_idx
					_end = run_idx
				} else {
					_end = run_idx
				}
			} else {
				if _end != -1 {
					reverse(chars[_start : _end+1])
					_start = -1
					_end = -1
				}
			}
		}

		// anything remaining ?
		if _start != -1 {
			reverse(chars[_start : _end+1])
		}
	}
}

// L1 and L2 rules
func reorder_resolved_levels(storage *Storage, debug bool) {
	// Applies L1.

	should_reset := true
	chars := storage.chars

	for idx := len(chars) - 1; idx >= 0; idx-- {
		_ch := chars[idx]
		// L1. On each line, reset the embedding level of the following
		// characters to the paragraph embedding level:
		if _ch.orig == "B" || _ch.orig == "S" {
			// 1. Segment separators,
			// 2. Paragraph separators,
			chars[idx].level = storage.base_level
			should_reset = true
		} else if should_reset && newStringSet("BN", "WS")[_ch.orig] {
			// 3. Any sequence of whitespace characters preceding a segment
			// separator or paragraph separator
			// 4. Any sequence of white space characters at the end of the
			// line.
			chars[idx].level = storage.base_level
		} else {
			should_reset = false
		}
	}

	max_len := len(chars)

	// L2 should be per line
	// Calculates highest level and lowest odd level on the fly.

	line_start := 0
	line_end := 0
	highest_level := 0
	lowest_odd_level := EXPLICIT_LEVEL_LIMIT

	for idx := 0; idx < max_len; idx++ {
		_ch := chars[idx]

		// calc the levels
		char_level := _ch.level
		if char_level > highest_level {
			highest_level = char_level
		}

		if char_level%2 != 0 && char_level < lowest_odd_level {
			lowest_odd_level = char_level
		}

		if _ch.orig == "B" || idx == max_len-1 {
			line_end = idx
			// omit line breaks
			if _ch.orig == "B" {
				line_end -= 1
			}

			reverse_contiguous_sequence(chars, line_start, line_end,
				highest_level, lowest_odd_level)

			// reset for next line run
			line_start = idx + 1
			highest_level = 0
			lowest_odd_level = EXPLICIT_LEVEL_LIMIT
		}
	}

	if debug {
		debug_storage(storage, debug_params{})
	}
}

// Applies L4: mirroring
//
// See: http://unicode.org/reports/tr9/#L4
func apply_mirroring(storage *Storage, debug bool) {
	// L4. A character is depicted by a mirrored glyph if and only if (a) the
	// resolved directionality of that character is R, and (b) the
	// Bidi_Mirrored property value of that character is true.
	for i, _ch := range storage.chars {
		unichar := _ch.ch
		if _embedding_direction(_ch.level) == "R" {
			if m, ok := MIRRORED[unichar]; ok {
				storage.chars[i].ch = m
			}
		}
	}

	if debug {
		debug_storage(storage, debug_params{})
	}
}

// Accepts a utf-8 string.
//
// Set `upper_is_rtl` to true to treat upper case chars as strong "R"
// for debugging (default: false).
//
// Set `base_dir` to "L" or "R" to override the calculated base_level.
//
// Set `debug` to true to display (using sys.stderr) the steps taken with the
// algorithm.
//
// Returns the display layout, either as unicode or `encoding` encoded
// string.
func get_display(str string, upper_is_rtl bool, base_dir string, debug bool) (_ string, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("%v", r)
		}
	}()

	storage := &Storage{}

	var base_level int

	text := []rune(str)

	if base_dir == "" {
		base_level = get_base_level(text, upper_is_rtl)
	} else {
		base_level = PARAGRAPH_LEVELS[base_dir]
	}

	storage.base_level = base_level
	storage.base_dir = []string{"L", "R"}[base_level]

	get_embedding_levels(text, storage, upper_is_rtl, debug)
	explicit_embed_and_overrides(storage, debug)
	resolve_weak_types(storage, debug)
	resolve_neutral_types(storage, debug)
	resolve_implicit_levels(storage, debug)
	reorder_resolved_levels(storage, debug)
	apply_mirroring(storage, debug)

	chars := storage.chars

	display := make([]rune, len(chars))
	for i, _ch := range chars {
		display[i] = _ch.ch
	}

	return string(display), nil
}

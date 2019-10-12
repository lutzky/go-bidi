// Copyright (C) 2008-2010 Yaacov Zamir <kzamir_a_walla.co.il>,
// Copyright (C) 2010-2015 Meir kriheli <mkriheli@gmail.com>,
// Copyright (C) 2019 Google LLC
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

package bidi

import (
	"fmt"
	"io"
	"runtime"
	"strconv"
	"unicode"

	"golang.org/x/text/unicode/bidi"
)

var paragraphLevels = map[bidi.Class]int{bidi.L: 0, bidi.AL: 1, bidi.R: 1}

const explicitLevelLimit = 62

func leastGreaterOdd(x int) int {
	return (x + 1) | 1
}

func leastGreaterEven(x int) int {
	return (x + 2) & ^1
}

var bidiClassNames = map[bidi.Class]string{
	bidi.L:   "L",
	bidi.R:   "R",
	bidi.EN:  "EN",
	bidi.ES:  "ES",
	bidi.ET:  "ET",
	bidi.AN:  "AN",
	bidi.CS:  "CS",
	bidi.B:   "B",
	bidi.S:   "S",
	bidi.WS:  "WS",
	bidi.ON:  "ON",
	bidi.BN:  "BN",
	bidi.NSM: "NSM",
	bidi.AL:  "AL",

	bidi.Control: "Control",

	bidi.LRO: "LRO",
	bidi.RLO: "RLO",
	bidi.LRE: "LRE",
	bidi.RLE: "RLE",
	bidi.PDF: "PDF",
	bidi.LRI: "LRI",
	bidi.RLI: "RLI",
	bidi.FSI: "FSI",
	bidi.PDI: "PDI",
}

var x2x5Mappings = map[bidi.Class]struct {
	f func(int) int
	s bidi.Direction
}{
	bidi.RLE: {leastGreaterOdd, bidi.Neutral},
	bidi.LRE: {leastGreaterEven, bidi.Neutral},
	bidi.RLO: {leastGreaterOdd, bidi.RightToLeft},
	bidi.LRO: {leastGreaterEven, bidi.LeftToRight},
}

type run struct {
	sor, eor bidi.Direction
	start    int
	length   int
	Type     bidi.Class
}

type charData struct {
	r     rune
	level int
	Type  bidi.Class
	orig  bidi.Class
}

type storage struct {
	baseLevel   int
	baseDir     string
	chars       []charData
	debugWriter io.Writer
	runs        []run
}

// Added 'B' so X6 won't execute in that case and X8 will run its course
var x6Ignored = x2x5MappingsKeys().plus(bidi.BN, bidi.PDF, bidi.B)
var x9Removed = x2x5MappingsKeys().plus(bidi.BN, bidi.PDF)

func x2x5MappingsKeys() classSet {
	result := classSet{}
	for k := range x2x5Mappings {
		result[k] = true
	}
	return result
}

type classSet map[bidi.Class]bool

func newClassSet(s ...bidi.Class) classSet {
	result := classSet{}
	for _, x := range s {
		result[x] = true
	}
	return result
}

func (ss classSet) plus(s ...bidi.Class) classSet {
	result := newClassSet(s...)
	for x := range ss {
		result[x] = true
	}
	return result
}

func embeddingDirection(x int) bidi.Direction {
	return []bidi.Direction{bidi.LeftToRight, bidi.RightToLeft}[x%2]
}

// isUCS2 = sys.maxunicode == 65535
const isUCS2 = true
const surrogateMin = 0xd800
const surrogateMax = 0xdbff

type debugParams struct {
	baseInfo bool
	noChars  bool
	runs     bool
}

func (s *storage) debug(params debugParams) {
	w := s.debugWriter
	if w == nil {
		return
	}

	pc := make([]uintptr, 1)
	n := runtime.Callers(2, pc)
	if n == 0 {
		panic("Failed to get name of calling function")
	}
	frames := runtime.CallersFrames(pc)
	frame, _ := frames.Next()

	fmt.Fprintf(w, "in %s\n", frame.Function)

	if params.baseInfo {
		fmt.Fprintf(w, "  base level  : %d\n", s.baseLevel)
		fmt.Fprintf(w, "  base dir    : %s\n", s.baseDir)
	}

	if params.runs {
		fmt.Fprintf(w, "  runs        : %+v\n", s.runs)
	}

	if !params.noChars {
		output := "  Chars       : "
		for _, ch := range s.chars {
			output += string(ch.r)
		}
		fmt.Fprintf(w, output+"\n")

		output = "  Res. levels : "
		for _, ch := range s.chars {
			output += strconv.Itoa(ch.level)
		}
		fmt.Fprintln(w, output)

		types := make([]string, len(s.chars))
		for i, ch := range s.chars {
			types[i] = fmt.Sprintf("%-3s", bidiClassNames[ch.Type])
		}

		for i := 0; i < 3; i++ {
			if i != 0 {
				output = "                %s\n"
			} else {
				output = "  Res. types  : %s\n"
			}
			extraOutput := ""
			for _, t := range types {
				extraOutput += string(t[i])
			}
			fmt.Fprintf(w, output, extraOutput)
		}
	}

}

func bidirectional(r rune) bidi.Class {
	prop, _ := bidi.LookupRune(r)
	return prop.Class()
}

// getBaseLevel gets the paragraph base embedding level. Returns 0 for LTR,
// 1 for RTL.
//
// Set `upperIsRTL` to true to treat upper case chars as strong
// bidi.RightToLEft for debugging, or false for normal operation.
func getBaseLevel(text []rune, upperIsRTL bool) int {
	baseLevel := -1
	var prevSurrogate rune

	// P2
	for i, ch := range text {
		// surrogate in case of ucs2
		if isUCS2 && (surrogateMin <= ch) && (ch <= surrogateMax) {
			prevSurrogate = ch
		} else if prevSurrogate != 0 {
			text[i] += prevSurrogate
			prevSurrogate = 0
		}

		// treat upper as RTL ?
		if upperIsRTL && unicode.IsUpper(ch) {
			baseLevel = 1
			break
		}

		bidiType := bidirectional(ch)

		if bidiType == bidi.AL || bidiType == bidi.R {
			baseLevel = 1
			break
		} else if bidiType == bidi.L {
			baseLevel = 0
			break
		}
	}

	// P3
	if baseLevel == -1 {
		baseLevel = 0
	}

	return baseLevel
}

// Get the paragraph base embedding level and direction,
// set the storage to the array of chars
func (s *storage) getEmbeddingLevels(text []rune, upperIsRTL bool) {
	var prevSurrogate rune
	baseLevel := s.baseLevel
	var bidiType bidi.Class

	// preset the storage's chars
	for i, r := range text {
		if isUCS2 && (surrogateMin <= r) && (r <= surrogateMax) {
			prevSurrogate = r
			continue
		} else if prevSurrogate != 0 {
			text[i] += prevSurrogate
			prevSurrogate = 0
		}

		if upperIsRTL && unicode.IsUpper(r) {
			bidiType = bidi.R
		} else {
			bidiType = bidirectional(r)
		}

		s.chars = append(s.chars, charData{
			r:     r,
			level: baseLevel,
			Type:  bidiType,
			orig:  bidiType,
		})
	}
	s.debug(debugParams{baseInfo: true})
}

type levelsType struct {
	embeddingLevel      int
	directionalOverride bidi.Direction
}

func directionToClass(runType bidi.Direction) bidi.Class {
	if runType == bidi.RightToLeft {
		return bidi.R
	}
	return bidi.L
}

// Apply X1 to X9 rules of the unicode algorithm.
//
// See http://unicode.org/reports/tr9/#Explicit_Levels_and_Directions
func (s *storage) explicitEmbedAndOverrides() {
	overflowCounter := 0
	almostOverflowCounter := 0
	directionalOverride := bidi.Neutral
	var levels []levelsType

	// X1
	embeddingLevel := s.baseLevel

	for i, ch := range s.chars {
		bidiType := ch.Type

		levelFunc := x2x5Mappings[bidiType].f
		override := x2x5Mappings[bidiType].s

		if levelFunc != nil {
			// So this is X2 to X5
			// if we've past explicitLevelLimit, note it and do nothing

			if overflowCounter != 0 {
				overflowCounter++
				continue
			}

			newLevel := levelFunc(embeddingLevel)
			if newLevel < explicitLevelLimit {
				levels = append(levels, levelsType{embeddingLevel, directionalOverride})
				embeddingLevel, directionalOverride = newLevel, override
			} else if embeddingLevel == explicitLevelLimit-2 {
				// The new level is invalid, but a valid level can still be
				// achieved if this level is 60 and we encounter an RLE or
				// RLO further on.  So record that we 'almost' overflowed.
				almostOverflowCounter++
			} else {
				overflowCounter++
			}
		} else {
			// X6
			if !x6Ignored[bidiType] {
				s.chars[i].level = embeddingLevel
				if directionalOverride != bidi.Neutral {
					s.chars[i].Type = directionToClass(directionalOverride)
				}
			} else if bidiType == bidi.PDF {
				// X7
				if overflowCounter != 0 {
					overflowCounter--
				} else if almostOverflowCounter != 0 && embeddingLevel != explicitLevelLimit-1 {
					almostOverflowCounter--
				} else if len(levels) > 0 {
					lTmp := levels[len(levels)-1]
					embeddingLevel = lTmp.embeddingLevel
					directionalOverride = lTmp.directionalOverride
					levels = levels[:len(levels)-1]
				}
			} else if bidiType == bidi.B {
				// X8
				levels = nil
				overflowCounter = 0
				almostOverflowCounter = 0
				s.chars[i].level = s.baseLevel
				embeddingLevel = ch.level
				directionalOverride = bidi.Neutral
			}
		}
	}

	// Removes the explicit embeds and overrides of types
	// RLE, LRE, RLO, LRO, PDF, and BN. Adjusts extended chars
	// next and prev as well

	// Applies X9. See http://unicode.org/reports/tr9/#X9
	{
		tmp := s.chars
		s.chars = make([]charData, 0, len(tmp))
		for _, ch := range tmp {
			if !x9Removed[ch.Type] {
				s.chars = append(s.chars, ch)
			}
		}
	}

	s.calcLevelRuns()

	s.debug(debugParams{runs: true})
}

// Split the storage to run of char types at the same level.
//
// Applies X10. See http://unicode.org/reports/tr9/#X10
func (s *storage) calcLevelRuns() {
	// run level depends on the higher of the two levels on either side of
	// the boundary If the higher level is odd, the type is R; otherwise,
	// it is L

	s.runs = nil
	chars := s.chars

	// empty string ?
	if len(chars) == 0 {
		return
	}

	calcLevelRun := func(b_l int, b_r int) bidi.Direction {
		max := b_l
		if b_r > max {
			max = b_r
		}
		return []bidi.Direction{bidi.LeftToRight, bidi.RightToLeft}[max%2]
	}

	firstChar := chars[0]

	sor := calcLevelRun(s.baseLevel, firstChar.level)
	eor := bidi.Neutral

	runStart := 0
	runLength := 0

	prevLevel, prevType := firstChar.level, firstChar.Type

	var (
		currLevel int
		currType  bidi.Class
	)

	for _, ch := range chars {
		currLevel, currType = ch.level, ch.Type

		if currLevel == prevLevel {
			runLength++
		} else {
			eor = calcLevelRun(prevLevel, currLevel)
			s.runs = append(s.runs, run{sor: sor, eor: eor, start: runStart,
				Type: prevType, length: runLength})
			sor = eor
			runStart += runLength
			runLength = 1
		}

		prevLevel, prevType = currLevel, currType
	}

	// for the last char/runlevel
	eor = calcLevelRun(currLevel, s.baseLevel)
	s.runs = append(s.runs, run{sor: sor, eor: eor, start: runStart,
		Type: currType, length: runLength})
}

// Resolve weak type rules W1 - W3.
//
// See: http://unicode.org/reports/tr9/#Resolving_Weak_Types
func (s *storage) resolveWeakTypes() {
	for _, run := range s.runs {
		prevStrong := directionToClass(run.sor)
		prevType := directionToClass(run.sor)
		start, length := run.start, run.length
		chars := s.chars[start : start+length]
		for i, ch := range chars {
			// W1. Examine each nonspacing mark (NSM) in the level run, and
			// change the type of the NSM to the type of the previous character.
			// If the NSM is at the start of the level run, it will get the type
			// of sor.
			bidiType := ch.Type

			if bidiType == bidi.NSM {
				bidiType = prevType
				chars[i].Type = bidiType
			}

			// W2. Search backward from each instance of a European number until
			// the first strong type (R, L, AL, or sor) is found. If an AL is
			// found, change the type of the European number to Arabic number.
			if bidiType == bidi.EN && prevStrong == bidi.AL {
				chars[i].Type = bidi.AL
			}

			// update prevStrong if needed
			if newClassSet(bidi.R, bidi.L, bidi.AL)[bidiType] {
				prevStrong = bidiType
			}

			prevType = ch.Type
		}

		// W3. Change all ALs to R
		for i, ch := range chars {
			if ch.Type == bidi.AL {
				chars[i].Type = bidi.R
			}
		}

		// W4. A single European separator between two European numbers changes
		// to a European number. A single common separator between two numbers of
		// the same type changes to that type.
		for idx := 1; idx < len(chars)-1; idx++ {
			bidiType := chars[idx].Type
			prevType := chars[idx-1].Type
			nextType := chars[idx+1].Type

			if bidiType == bidi.ES && prevType == bidi.EN && nextType == bidi.EN {
				chars[idx].Type = bidi.EN
			}

			if bidiType == bidi.CS && prevType == nextType &&
				(prevType == bidi.AL || prevType == bidi.EN) {
				chars[idx].Type = prevType
			}
		}

		// W5. A sequence of European terminators adjacent to European numbers
		// changes to all European numbers.
		for idx := range chars {
			if chars[idx].Type == bidi.EN {
				for etIdx := idx - 1; etIdx > -1; etIdx-- {
					if chars[etIdx].Type == bidi.ET {
						chars[etIdx].Type = bidi.EN
					} else {
						break
					}
				}
				for etIdx := idx + 1; etIdx < len(chars); etIdx++ {
					if chars[etIdx].Type == bidi.ET {
						chars[etIdx].Type = bidi.EN
					} else {
						break
					}
				}
			}
		}

		// W6. Otherwise, separators and terminators change to Other Neutral.
		for i, ch := range chars {
			if newClassSet(bidi.ET, bidi.ES, bidi.CS)[ch.Type] {
				chars[i].Type = bidi.ON
			}
		}

		// W7. Search backward from each instance of a European number until the
		// first strong type (R, L, or sor) is found. If an L is found, then
		// change the type of the European number to L.
		prevStrong = directionToClass(run.sor)
		for i, ch := range chars {
			if ch.Type == bidi.EN && prevStrong == bidi.L {
				chars[i].Type = bidi.L
			}

			if ch.Type == bidi.L || ch.Type == bidi.R {
				prevStrong = ch.Type
			}
		}
	}

	s.debug(debugParams{runs: true})
}

// Resolving neutral types. Implements N1 and N2
//
// See: http://unicode.org/reports/tr9/#Resolving_Neutral_Types
func (s *storage) resolveNeutralTypes() {
	for _, run := range s.runs {
		start := run.start
		length := run.length
		// use sor and eor
		chars := []charData{{Type: directionToClass(run.sor)}}
		for _, ch := range s.chars[start : start+length] {
			chars = append(chars, ch)
		}
		chars = append(chars, charData{Type: directionToClass(run.eor)})
		totalChars := len(chars)

		var prevBidiType, nextBidiType bidi.Class

		seqStart := -1
		for idx := 0; idx < totalChars; idx++ {
			ch := chars[idx]
			if newClassSet(bidi.B, bidi.S, bidi.WS, bidi.ON)[ch.Type] {
				// N1. A sequence of neutrals takes the direction of the
				// surrounding strong text if the text on both sides has the same
				// direction. European and Arabic numbers act as if they were R
				// in terms of their influence on neutrals. Start-of-level-run
				// (sor) and end-of-level-run (eor) are used at level run
				// boundaries.
				if seqStart == -1 {
					seqStart = idx
					prevBidiType = chars[idx-1].Type
				}
			} else {
				if seqStart != -1 {
					nextBidiType = chars[idx].Type

					if prevBidiType == bidi.AN || prevBidiType == bidi.EN {
						prevBidiType = bidi.R
					}

					if nextBidiType == bidi.AN || nextBidiType == bidi.EN {
						nextBidiType = bidi.R
					}

					for seqIdx := seqStart; seqIdx < idx; seqIdx++ {
						if prevBidiType == nextBidiType {
							s.chars[start+seqIdx-1].Type = prevBidiType
						} else {
							// N2. Any remaining neutrals take the embedding
							// direction. The embedding direction for the given
							// neutral character is derived from its embedding
							// level: L if the character is set to an even level,
							// and R if the level is odd.
							s.chars[start+seqIdx-1].Type =
								directionToClass(embeddingDirection(chars[seqIdx].level))
						}
					}

					seqStart = -1
				}
			}
		}
	}

	s.debug(debugParams{})
}

// Resolving implicit levels (I1, I2)
//
// See: http://unicode.org/reports/tr9/#Resolving_Implicit_Levels
func (s *storage) resolveImplicitLevels() {
	for _, run := range s.runs {
		start, length := run.start, run.length
		chars := s.chars[start : start+length]

		for i, ch := range chars {
			// only those types are allowed at this stage
			if !newClassSet(bidi.L, bidi.R, bidi.EN, bidi.AL)[ch.Type] {
				panic(bidiClassNames[ch.Type] + " not allowed here")
			}

			if embeddingDirection(ch.level) == bidi.LeftToRight {
				// I1. For all characters with an even (left-to-right) embedding
				// direction, those of type R go up one level and those of type
				// AN or EN go up two levels.
				if ch.Type == bidi.R {
					chars[i].level++
				} else if ch.Type != bidi.L {
					chars[i].level += 2
				}
			} else {
				// I2. For all characters with an odd (right-to-left) embedding
				// direction, those of type L, EN or AN  go up one level.
				if ch.Type != bidi.R {
					chars[i].level++
				}
			}
		}
	}

	s.debug(debugParams{runs: true})
}

func reverse(x []charData) {
	for i := 0; i < len(x)/2; i++ {
		opp := len(x) - i - 1
		x[i], x[opp] = x[opp], x[i]
	}
}

// L2. From the highest level found in the text to the lowest odd
// level on each line, including intermediate levels not actually
// present in the text, reverse any contiguous sequence of characters
// that are at that level or higher.
func reverseContiguousSequence(chars []charData, lineStart, lineEnd, highestLevel,
	lowestOddLevel int) {
	for level := highestLevel; level > lowestOddLevel-1; level-- {
		start := -1
		end := -1

		for runIdx := lineStart; runIdx < lineEnd+1; runIdx++ {
			runCh := chars[runIdx]

			if runCh.level >= level {
				if start == -1 {
					start = runIdx
					end = runIdx
				} else {
					end = runIdx
				}
			} else {
				if end != -1 {
					reverse(chars[start : end+1])
					start = -1
					end = -1
				}
			}
		}

		// anything remaining ?
		if start != -1 {
			reverse(chars[start : end+1])
		}
	}
}

// L1 and L2 rules
func (s *storage) reorderResolvedLevels() {
	// Applies L1.

	shouldReset := true
	chars := s.chars

	for idx := len(chars) - 1; idx >= 0; idx-- {
		ch := chars[idx]
		// L1. On each line, reset the embedding level of the following
		// characters to the paragraph embedding level:
		if ch.orig == bidi.B || ch.orig == bidi.S {
			// 1. Segment separators,
			// 2. Paragraph separators,
			chars[idx].level = s.baseLevel
			shouldReset = true
		} else if shouldReset && newClassSet(bidi.BN, bidi.WS)[ch.orig] {
			// 3. Any sequence of whitespace characters preceding a segment
			// separator or paragraph separator
			// 4. Any sequence of white space characters at the end of the
			// line.
			chars[idx].level = s.baseLevel
		} else {
			shouldReset = false
		}
	}

	maxLen := len(chars)

	// L2 should be per line
	// Calculates highest level and lowest odd level on the fly.

	lineStart := 0
	lineEnd := 0
	highestLevel := 0
	lowestOddLevel := explicitLevelLimit

	for idx := 0; idx < maxLen; idx++ {
		ch := chars[idx]

		// calc the levels
		charLevel := ch.level
		if charLevel > highestLevel {
			highestLevel = charLevel
		}

		if charLevel%2 != 0 && charLevel < lowestOddLevel {
			lowestOddLevel = charLevel
		}

		if ch.orig == bidi.B || idx == maxLen-1 {
			lineEnd = idx
			// omit line breaks
			if ch.orig == bidi.B {
				lineEnd--
			}

			reverseContiguousSequence(chars, lineStart, lineEnd,
				highestLevel, lowestOddLevel)

			// reset for next line run
			lineStart = idx + 1
			highestLevel = 0
			lowestOddLevel = explicitLevelLimit
		}
	}

	s.debug(debugParams{})
}

// applyMirroring applies L4: mirroring
//
// See: http://unicode.org/reports/tr9/#L4
func (s *storage) applyMirroring() {
	// L4. A character is depicted by a mirrored glyph if and only if (a) the
	// resolved directionality of that character is R, and (b) the
	// Bidi_Mirrored property value of that character is true.
	for i, ch := range s.chars {
		if embeddingDirection(ch.level) == bidi.RightToLeft {
			if m, ok := mirrored[ch.r]; ok {
				s.chars[i].r = m
			}
		}
	}

	s.debug(debugParams{})
}

// getDisplay performs the unicode logical-to-visual algorithm on str.
//
// If debug is not nil, the steps taken with the algorithm will be written
// to it.
func getDisplay(str string, upperIsRTL bool, baseDir bidi.Direction, debug io.Writer) (_ string, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("%v", r)
		}
	}()

	s := &storage{debugWriter: debug}

	var baseLevel int

	text := []rune(str)

	if baseDir == bidi.LeftToRight || baseDir == bidi.RightToLeft {
		baseLevel = paragraphLevels[directionToClass(baseDir)]
	} else {
		baseLevel = getBaseLevel(text, upperIsRTL)
	}

	s.baseLevel = baseLevel
	s.baseDir = []string{"L", "R"}[baseLevel]

	s.getEmbeddingLevels(text, upperIsRTL)
	s.explicitEmbedAndOverrides()
	s.resolveWeakTypes()
	s.resolveNeutralTypes()
	s.resolveImplicitLevels()
	s.reorderResolvedLevels()
	s.applyMirroring()

	chars := s.chars

	display := make([]rune, len(chars))
	for i, ch := range chars {
		display[i] = ch.r
	}

	return string(display), nil
}

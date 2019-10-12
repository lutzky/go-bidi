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

// Package bidi implements the bidi algorithm for converting logical-order
// strings to visual-order strings, using the Unicode algorithm. This is a
// complementary package to golang.org/x/text/unicode, which does not yet
// contain this functionality.
package bidi

import (
	"io"

	"golang.org/x/text/unicode/bidi"
)

// Displayer provides a configurable object for running Display. For typical usage,
// use the ordinary Display function instead.
type Displayer struct {
	// Treat uppercase characters as RTL
	UpperIsRTL bool

	// Base direction. Default is bidi.Neutral
	BaseDir bidi.Direction

	// If not nil, debug output will be written here
	Debug io.Writer
}

// Display returns the visual order for str, which should be in logical order,
// using the configuration in d.
func (d Displayer) Display(str string) (string, error) {
	return getDisplay(str, d.UpperIsRTL, d.BaseDir, d.Debug)
}

// Display returns the visual order for str, which should be in logical order.
func Display(str string) (string, error) {
	return Displayer{
		BaseDir: bidi.Neutral,
	}.Display(str)
}

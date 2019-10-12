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
	"io"

	"golang.org/x/text/unicode/bidi"
)

type Displayer struct {
	UpperIsRtl bool
	BaseDir    bidi.Direction
	Debug      io.Writer
}

func (d Displayer) Display(str string) (_ string, err error) {
	return get_display(str, d.UpperIsRtl, d.BaseDir, d.Debug)
}

func Display(str string) (string, error) {
	return Displayer{
		BaseDir: bidi.Neutral,
	}.Display(str)
}

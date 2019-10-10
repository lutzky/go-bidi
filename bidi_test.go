package bidi

import (
	"strings"
	"testing"
)

type testCase struct {
	input, want string
}

func TestImplicitWithUpperIsRTL(t *testing.T) {
	testCases := []testCase{
		{`car is THE CAR in arabic`, `car is RAC EHT in arabic`},
		{`CAR IS the car IN ENGLISH`, `HSILGNE NI the car SI RAC`},
		{`he said "IT IS 123, 456, OK"`, `he said "KO ,456 ,123 SI TI"`},
		{`he said "IT IS (123, 456), OK"`,
			`he said "KO ,(456 ,123) SI TI"`},
		{`he said "IT IS 123,456, OK"`, `he said "KO ,123,456 SI TI"`},
		{`he said "IT IS (123,456), OK"`,
			`he said "KO ,(123,456) SI TI"`},
		{`HE SAID "it is 123, 456, ok"`, `"it is 123, 456, ok" DIAS EH`},
		{`<H123>shalom</H123>`, `<123H/>shalom<123H>`},
		{`<h123>SAALAM</h123>`, `<h123>MALAAS</h123>`},
		{`HE SAID "it is a car!" AND RAN`,
			`NAR DNA "!it is a car" DIAS EH`},
		{`HE SAID "it is a car!x" AND RAN`,
			`NAR DNA "it is a car!x" DIAS EH`},
		{`SOLVE 1*5 1-5 1/5 1+5`, `1+5 1/5 1-5 5*1 EVLOS`},
		{`THE RANGE IS 2.5..5`, `5..2.5 SI EGNAR EHT`},
		{`-2 CELSIUS IS COLD`, `DLOC SI SUISLEC 2-`},
	}

	runTestCases(t, testCases)
}

// Test for the case reported in
// https://github.com/MeirKriheli/python-bidi/issues/10
func TestMixedHebrewNumbersIssue10(t *testing.T) {
	testCases := []testCase{
		{"1 2 3 \u05E0\u05D9\u05E1\u05D9\u05D5\u05DF", "\u05DF\u05D5\u05D9\u05E1\u05D9\u05E0 3 2 1"},
		{"1 2 3 123 \u05E0\u05D9\u05E1\u05D9\u05D5\u05DF", "\u05DF\u05D5\u05D9\u05E1\u05D9\u05E0 123 3 2 1"},
	}

	runTestCases(t, testCases)
}

func runTestCases(t *testing.T, testCases []testCase) {
	t.Helper()

	f := func(s string, debug bool) (string, error) {
		return get_display(s,
			/*upper_is_rtl=*/ true,
			/*base_dir=*/ "",
			debug,
		)
	}

	for _, tc := range testCases {
		got, err := f(tc.input, false)
		if err != nil {
			f(tc.input, true)
			t.Fatalf("get_display(%q) returned error", tc.input)
		}
		if got != tc.want {
			f(tc.input, true)
			t.Fatalf("get_display(%q) = %q; want %q", tc.input, got, tc.want)
		}
	}
}

func TestOverrideBaseDir(t *testing.T) {
	// normally the display should be :MOLAHS be since we're overriding the
	// base dir the colon should be at the end of the display
	input := "SHALOM:"
	want := "MOLAHS:"

	got, err := get_display(input,
		/*upper_is_rtl=*/ true,
		/*base_dir=*/ "L",
		/*debug=*/ false,
	)

	if err != nil {
		t.Fatalf("Error: %v", err)
	}

	if got != want {
		t.Errorf("got %q; want %q", got, want)
	}
}

func TestExplicitWithUpperIsRTL(t *testing.T) {
	testCases := []testCase{
		{"this is _LJUST_o", "this is JUST"},
		{"a _lsimple _RteST_o th_oat", "a simple TSet that"},
		{"HAS A _LPDF missing", "PDF missing A SAH"},
		{"AnD hOw_L AbOuT, 123,987 tHiS_o",
			"w AbOuT, 123,987 tHiSOh DnA"},
		{"a GOOD - _L_oTEST.", "a TSET - DOOG."},
		{"here_L is_o_o_o _R a good one_o", "here is eno doog a "},
		{"THE _rbest _lONE and", "best ENO and EHT"},
		{"A REAL BIG_l_o BUG!", "!GUB GIB LAER A"},
		{"a _L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_L_Rbug",
			"a gub"},
		// FIXME the following commented explicit test fails
		// {"AN ARABIC _l_o 123-456 NICE ONE!",
		//  "!ENO ECIN 456-123  CIBARA NA"},
		{"AN ARABIC _l _o 123-456 PAIR", "RIAP   123-456 CIBARA NA"},
		{"this bug 67_r_o89 caught!", "this bug 6789 caught!"},
	}

	// adopt fribidi's CapRtl encoding
	mappings := map[string]string{
		"_>": "\u200E", // LRM
		"_<": "\u200F", // RLM
		"_l": "\u202A", // LRE
		"_r": "\u202B", // RLE
		"_o": "\u202C", // PDF
		"_L": "\u202D", // LRO
		"_R": "\u202E", // RLO
		"__": "_",
	}

	for _, tc := range testCases {
		input := tc.input
		want := tc.want
		for from, to := range mappings {
			input = strings.ReplaceAll(input, from, to)
			want = strings.ReplaceAll(want, from, to)
		}
		got, err := get_display(
			input,
			/*upper_is_rtl=*/ true,
			/*base_dir=*/ "",
			/*debug=*/ false,
		)
		if err != nil {
			t.Fatalf("Error converting %q: %v", tc.input, err)
		}
		if got != want {
			t.Fatalf("Converting %q got %q; wanted %q", input, got, want)
		}
	}
}

// Test for storage and base levels in case of surrogate pairs
func TestSurrogate(t *testing.T) {
	storage := Storage{}

	text := "HELLO \U0001d7f612"
	get_embedding_levels(
		[]rune(text),
		&storage,
		/*upper_is_rtl=*/ true,
		/*debug=*/ false,
	)

	// should return 9, not 10 even in --with-unicode=ucs2
	if len(storage.chars) != 9 {
		t.Fatalf("len(storage.chars) == %d; want 9", len(storage.chars))
	}

	// Is the expected result ? should be EN
	_ch := storage.chars[6]
	wantChar := '\U0001d7f6'
	if _ch.ch != wantChar {
		t.Errorf("storage.chars[6] = %U; want %U", _ch.ch, wantChar)
	}
	if _ch.Type != "EN" {
		t.Errorf("storage.chars[6].Type = %q; want EN", _ch.Type)
	}

	want := "\U0001d7f612 OLLEH"
	got, err := get_display(text, true, "", false)
	if err != nil {
		t.Fatalf("Got error: %v", err)
	}
	if got != want {
		t.Errorf("got %q; want %q", got, want)
	}
}

func BenchmarkSimple(b *testing.B) {
	input := "לקראת סוף המאה ה-19"
	for i := 0; i < b.N; i++ {
		get_display(input, false, "", false)
	}
}

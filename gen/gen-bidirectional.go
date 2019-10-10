package main

import (
	"bufio"
	"bytes"
	"fmt"
	"go/format"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"text/template"
)

var tmpl = template.Must(template.New("").Parse(`package bidi

var Bidirectional = map[rune]string{
{{- range .}}
	'{{.CodePoint}}': "{{.BidiType}}",
{{- end}}
}
`))

type dataLine struct {
	CodePoint, BidiType string
}

func main() {
	unicodeData, err := os.Open("UnicodeData.txt")
	if err != nil {
		log.Fatalf("Failed to open UnicodeData.txt: %v", err)
	}
	defer unicodeData.Close()

	var data []dataLine

	scanner := bufio.NewScanner(unicodeData)
	line := 0
	for scanner.Scan() {
		line++
		s := strings.Split(scanner.Text(), ";")
		if len(s) < 5 {
			log.Fatalf("Line %d has only %d fields", line, len(s))
		}
		if s[2] == "Cs" {
			// surrogate half
			continue
		}
		var codePoint string
		if len(s[0]) <= 4 {
			codePoint = fmt.Sprintf("\\u%04s", s[0])
		} else {
			codePoint = fmt.Sprintf("\\U%08s", s[0])
		}
		data = append(data, dataLine{codePoint, s[4]})
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Failed to read lines: %v", err)
	}

	var buf bytes.Buffer

	if err := tmpl.Execute(&buf, data); err != nil {
		log.Fatalf("Error executing template: %v", err)
	}

	formatted, err := format.Source(buf.Bytes())
	if err != nil {
		log.Fatalf("Error formatting result: %v", err)
	}

	if err := ioutil.WriteFile("bidirectional.go", formatted, 0644); err != nil {
		log.Fatalf("Error writing bidirectional.go: %v", err)
	}
}

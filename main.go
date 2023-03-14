package main

import (
	"fmt"
	"glox/ast"
	"glox/parser"
	"glox/scanner"
	"os"
	g_err "glox/error"
)

var PROGRAM_PATH string
var SOURCE_CODE string

func run() {
	file, err := os.ReadFile(PROGRAM_PATH)
	if err != nil { os.Exit(1) }
	SOURCE_CODE = string(file)
	scanner := scanner.New(&PROGRAM_PATH, &SOURCE_CODE)
	parser := parser.New(&PROGRAM_PATH, scanner.Tokens())
	interp := ast.New(&PROGRAM_PATH, parser.Parse())
	interp.Interpret()
}

func main() {
	args := len(os.Args)
	switch {
	case args > 2:
		fmt.Println("usage: glox [script]")
		os.Exit(64)
	case args == 2:
		PROGRAM_PATH = os.Args[1]
		run()
		if g_err.SCAN_ERROR || g_err.PARSE_ERROR { os.Exit(65) }
		if g_err.RUNTIME_ERROR { os.Exit(70) }
	default:
		fmt.Println("expected at least one input file")
	}
}
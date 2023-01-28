package main

import (
	"fmt"
	gloxError "glox/error"
	"glox/parser"
	"glox/scanner"
	"glox/ast"
	"os"
)

var PROGRAM_PATH string
var SOURCE_CODE string

func run() {
	file, err := os.ReadFile(PROGRAM_PATH)
	if err != nil { os.Exit(1) }
	SOURCE_CODE = string(file)
	scanner, err := scanner.New(&PROGRAM_PATH, &SOURCE_CODE)
	if err != nil { return }
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
		if gloxError.LEX_ERROR || gloxError.PARSE_ERROR { os.Exit(65) }
		if gloxError.RUNTIME_ERROR { os.Exit(70) }
	default:
		fmt.Println("expected at least one input file")
	}
}
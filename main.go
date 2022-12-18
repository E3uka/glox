package main

import (
	"fmt"
	gloxError "glox/error"
	"glox/parser"
	"glox/scanner"
	"os"
)

var PROGRAM_PATH string
var SOURCE_CODE string

func run() {
	file, err := os.ReadFile(PROGRAM_PATH)
	if err != nil {
		os.Exit(1)
	}
	SOURCE_CODE = string(file)
	scn, err := scanner.New(&PROGRAM_PATH, &SOURCE_CODE)
	if err != nil {
		return
	}
	parser := parser.New(&PROGRAM_PATH, scn.Tokens()).Parse()
	fmt.Printf("%#v\n", parser)
	// interpreter := ast.New(&PROGRAM_PATH)
	/*
		fmt.Printf("%#v\n", expr)
		fmt.Println(ast.AstPrinter{}.Print(expr))
		fmt.Printf("%#v\n", interpreter)
	*/
	// interpreter.Interpret()
}

func main() {
	if len(os.Args) > 2 {
		fmt.Println("usage: glox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		PROGRAM_PATH = os.Args[1]
		run()
		if gloxError.LEX_ERROR || gloxError.PARSE_ERROR {
			os.Exit(65)
		}
		if gloxError.RUNTIME_ERROR {
			os.Exit(70)
		}
	} else {
		fmt.Println("expected at least one input file")
	}
}

package main

import (
	"fmt"
	"glox/ast"
	gloxError "glox/error"
	"glox/parser"
	"glox/scanner"
	"os"
)

func runFile(path string) {
	file, err := os.ReadFile(path)
	if err != nil {
		os.Exit(1)
	}

	source := string(file)
	tokScanner := scanner.New(source)
	parser := parser.New(tokScanner.ScanTokens())
	if expression, err := parser.Parse(); err != nil {
		return
	} else {
		fmt.Println(ast.AstPrinter{}.Print(expression))
	}
}

func runPrompt() {
	fmt.Println("expected at least one input file")
}

// TODO: implement a REPL later.
func main() {
	if len(os.Args) > 2 {
		fmt.Println("Usage: glox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		runFile(os.Args[1])
		if gloxError.GlobalError {
			os.Exit(64)
		}
	} else {
		runPrompt()
	}
}

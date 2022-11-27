package main

import (
	"fmt"
	"glox/ast"
	gloxError "glox/error"
	"glox/lexer"
	"glox/parser"
	"os"
)

var GlobalPath string

func run() {
	file, err := os.ReadFile(GlobalPath)
	if err != nil {
		os.Exit(1)
	}

	source := string(file)
	tokens, err := lexer.New(&GlobalPath, &source).LexTokens()
	if err != nil {
		return
	}
	parser := parser.New(&GlobalPath, &tokens)
	expression, err := parser.Parse()
	if err != nil {
		return
	}
	fmt.Printf("%#v\n", expression)
	fmt.Println(ast.AstPrinter{}.Print(expression))
}

// TODO: implement a REPL later.
func main() {
	if len(os.Args) > 2 {
		fmt.Println("usage: glox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		GlobalPath = os.Args[1]
		run()
		if gloxError.GlobalError {
			os.Exit(64)
		}
	} else {
		fmt.Println("expected at least one input file")
	}
}

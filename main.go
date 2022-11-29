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
	lexer, err := lexer.New(&GlobalPath, &source)
	if err != nil {
		return
	}
	/*
		parser, err := parser.New(&GlobalPath, lexer.Tokens())
		if err != nil {
			return
		}
		fmt.Printf("%#v\n", *parser.Expr())
		fmt.Println(ast.AstPrinter{}.Print(*parser.Expr()))
	*/

	pratt := parser.NewPratt(&GlobalPath, lexer.Tokens())
	//fmt.Printf("%#v\n", pratt.Expr())
	fmt.Println(ast.AstPrinter{}.Print(pratt.Expr()))
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

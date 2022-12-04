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
	parser := parser.NewPrattParser(&GlobalPath, lexer.Tokens())
	interpreter := ast.NewInterpreter(parser.Expr())
	//fmt.Printf("%#v\n", parser.Expr())
	fmt.Println(ast.AstPrinter{}.Print(parser.Expr()))
	fmt.Println(interpreter.Interpret())
}

// TODO: implement a REPL later.
func main() {
	if len(os.Args) > 2 {
		fmt.Println("usage: glox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		GlobalPath = os.Args[1]
		run()
		if gloxError.GLOBAL_ERROR {
			os.Exit(64)
		}
	} else {
		fmt.Println("expected at least one input file")
	}
}

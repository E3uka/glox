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
	expr := parser.Parse()
	interpreter := ast.NewInterpreter(&GlobalPath, expr)
	//fmt.Printf("%#v\n", expr)
	//fmt.Println(ast.AstPrinter{}.Print(expr))
	interpreter.Interpret()
	//fmt.Printf("%#v\n", interpreter)
}

// TODO: implement a REPL later.
func main() {
	if len(os.Args) > 2 {
		fmt.Println("usage: glox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		GlobalPath = os.Args[1]
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

package main

import (
	"fmt"
	gloxError "glox/error"
	"glox/scanner"
	"os"
)

func runFile(path string) {
	file, err := os.ReadFile(path)
	if err != nil {
		os.Exit(1)
	}

	source := string(file)
	sourceScanner := scanner.New(source)
	sourceScanner.ScanTokens()

	fmt.Printf("%+v\n", sourceScanner) // uncomment for sourceScanner output
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

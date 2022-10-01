package main

import (
	"fmt"
	"glox/error"
	"os"
)

func runFile(path string) {
	file, err := os.ReadFile(path)
	if err != nil {
		os.Exit(1)
	}

	fmt.Println(string(file))

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
		if error.GlobalError {
			os.Exit(64)
		}
	} else {
		runPrompt()
	}
}

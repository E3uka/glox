package main

import (
	"fmt"
	"os"
)

var GlobalError bool = false

type error struct {
	reason   string
	position uint
}

// It's good engineering practive to separate the code that separate the code
// that generates errors from the code that reports them.
func (e error) report() string {
	GlobalError = true
	return fmt.Sprintf("[line: %d] Error: %s\n", e.position, e.reason)
}

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
		if GlobalError {
			os.Exit(64)
		}
	} else {
		runPrompt()
	}
}

package error

import "fmt"

var GlobalError bool = false

func Error(line int, reason string) {
	report(line, "", reason)
}

// It's good engineering practive to separate the code that separate the code
// that generates errors from the code that reports them.
func report(line int, location, reason string) {
	GlobalError = true
	fmt.Printf("[line: %d] Error %s: %s\n", line, location, reason)
}

package error

import "fmt"

var GlobalError bool = false

func Error(line int, reason string) {
	report(line, "", reason)
}

func report(line int, location, reason string) {
	GlobalError = true
	fmt.Printf("[line: %d] Error %s: %s\n", line, location, reason)
}

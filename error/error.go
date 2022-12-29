package error

import (
	"errors"
	"fmt"
)

var LEX_ERROR bool = false
var PARSE_ERROR bool = false
var RUNTIME_ERROR bool = false

func ScanError(path *string, line int, reason string) error {
	LEX_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s\n", *path, line, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func ParsePanic(path *string, line int, reason string) {
	msg := fmt.Sprintf("%s:%d: %s", *path, line, reason)
	panic(msg)
}

func ParsePanicRecover(reason string) {
	PARSE_ERROR = true
	fmt.Println(reason)
}

func RuntimePanic(path *string, reason string, expr ...interface{}) {
	msg := fmt.Sprintf("%s: %v: %s", *path, expr, reason)
	panic(msg)
}

func RuntimePanicRecover(reason string) {
	RUNTIME_ERROR = true
	fmt.Println(reason)
}

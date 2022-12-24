package error

import (
	"errors"
	"fmt"
)

var LEX_ERROR bool = false
var PARSE_ERROR bool = false
var RUNTIME_ERROR bool = false

func Lex_Error(path *string, line int, reason string) error {
	LEX_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s\n", *path, line, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func Parse_Panic(path *string, line int, reason string) {
	msg := fmt.Sprintf("%s:%d: %s", *path, line, reason)
	panic(msg)
}

func Parse_Panic_Recover(reason string) {
	PARSE_ERROR = true
	fmt.Println(reason)
}

func Runtime_Panic(path *string, reason string, expr ...interface{}) {
	msg := fmt.Sprintf("%s: %v: %s", *path, expr, reason)
	panic(msg)
}

func Runtime_Panic_Recover(reason string) {
	RUNTIME_ERROR = true
	fmt.Println(reason)
}

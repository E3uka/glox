package error

import (
	"errors"
	"fmt"
	"glox/token"
)

var GLOBAL_ERROR bool = false
var PARSE_ERROR bool = false
var RUNTIME_ERROR bool = false

func LexError(path *string, line int, reason string) error {
	GLOBAL_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s\n", *path, line, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func ParsePanic(path *string, tok token.Token, reason string) {
	msg := fmt.Sprintf("%s:%d: %s: %s", *path, tok.Line, tok.Literal, reason)
	panic(msg)
}

func ParsePanicRecover(reason string) {
	PARSE_ERROR = true
	fmt.Println(reason)
}

func RuntimeError(path *string, reason string, expr ...interface{}) {
	RUNTIME_ERROR = true
	msg := fmt.Sprintf("%s: %v: %s\n", *path, expr, reason)
	panic(msg)
}

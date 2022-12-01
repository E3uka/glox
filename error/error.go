package error

import (
	"errors"
	"fmt"
	"glox/token"
)

var GLOBAL_ERROR bool = false

func LexError(path *string, line int, reason string) error {
	GLOBAL_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s\n", *path, line, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func ParseError(path *string, tok token.Token, reason string) error {
	GLOBAL_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s: %s\n", *path, tok.Line, tok.Lexeme, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func ParsePanic(path *string, tok token.Token, reason string) error {
	GLOBAL_ERROR = true
	msg := fmt.Sprintf("%s:%d: %s: %s", *path, tok.Line, tok.Lexeme, reason)
	panic(msg)
}

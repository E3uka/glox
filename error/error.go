package error

import (
	"errors"
	"fmt"
	"glox/token"
)

var GlobalError bool = false

func LexError(path *string, line int, reason string) error {
	GlobalError = true
	msg := fmt.Sprintf("%s:%d: %s\n", *path, line, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func ParseError(path *string, tok token.Token, reason string) error {
	GlobalError = true
	msg := fmt.Sprintf("%s:%d: %s: %s\n", *path, tok.Line, tok.Lexeme, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

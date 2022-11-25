package error

import (
	"errors"
	"fmt"
	"glox/token"
)

var GlobalError bool = false

func ScanError(line int, reason string) error {
	GlobalError = true
	msg := fmt.Sprintf("%d: %s\n", line, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func ParseError(tok token.Token, reason string) error {
	GlobalError = true
	msg := fmt.Sprintf("%d: %s: %s\n", tok.Line, tok.Lexeme, reason)
	fmt.Printf(msg)
	return errors.New(msg)
}

func Error(line int, reason string) {
	report(line, "", reason)
}

func report(line int, location, reason string) {
	GlobalError = true
	fmt.Printf("[line: %d] Error %s: %s\n", line, location, reason)
}

func ReportParserError(tok token.Token, msg string) {
	if tok.TokType == token.EOF {
		report(tok.Line, " at end", msg)
	} else {
		report(tok.Line, " at '"+tok.Lexeme+"'", msg)
	}
}

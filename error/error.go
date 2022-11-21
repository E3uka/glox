package error

import (
	"fmt"
	"glox/token"
)

var GlobalError bool = false

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

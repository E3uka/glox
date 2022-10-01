package token

import (
	"fmt"
	"strconv"
)

type TokenType uint

const (
	// Special Tokens (trick learned from go source): go/src/go/token/token.go
	ILLEGAL TokenType = iota
	EOF
	COMMENT // //

	literal_beg
	// Literals
	IDENT  // main
	NUMBER // 123.45
	STRING // "lets go"
	literal_end

	operator_beg
	// Operators and delimiters
	LPAREN // (
	LBRACK // [
	LBRACE // {
	COMMA  // ,
	PERIOD // .

	RPAREN    // )
	RBRACK    // ]
	RBRACE    // }
	SEMICOLON // ;
	COLON     // :

	ADD // +
	SUB // -
	MUL // *
	QUO // /

	NOT // !
	NEQ // !=
	LEQ // <=
	GEQ // >=

	ASSIGN // =
	EQL    // ==
	LSS    // <
	GTR    // >
	operator_end

	keyword_beg
	// Keywords
	AND
	CLASS
	ELSE
	FALSE
	FOR

	IF
	LET
	NIL
	OR
	PRINT

	RETURN
	SUPER
	THIS
	TRUE
	WHILE
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	COMMENT: "COMMENT",

	IDENT:  "IDENT",
	NUMBER: "NUMBER",
	STRING: "STRING",

	LPAREN: "(",
	LBRACK: "[",
	LBRACE: "{",
	COMMA:  ",",
	PERIOD: ".",

	RPAREN:    ")",
	RBRACK:    "]",
	RBRACE:    "}",
	SEMICOLON: ";",
	COLON:     ":",

	ADD: "+",
	SUB: "-",
	MUL: "*",
	QUO: "/",

	NOT: "!",
	NEQ: "!=",
	LEQ: "<=",
	GEQ: ">=",

	ASSIGN: "=",
	EQL:    "==",
	LSS:    "<",
	GTR:    ">",

	AND:   "and",
	CLASS: "class",
	ELSE:  "else",
	FALSE: "false",
	FOR:   "for",

	IF:    "if",
	LET:   "let",
	NIL:   "nil",
	OR:    "or",
	PRINT: "print",

	RETURN: "return",
	SUPER:  "super",
	THIS:   "this",
	TRUE:   "true",
	WHILE:  "while",
}

func (tt TokenType) String() string {
	s := ""
	if 0 <= tt && tt < TokenType(len(tokens)) {
		s = tokens[tt]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tt)) + ")"
	}
	return s
}

type Token struct {
	TokType TokenType
	Lexeme  string
	Literal interface{}
	Line    int
}

func (tok Token) String() string {
	return fmt.Sprintf("%s %s %s", tok.TokType, tok.Lexeme, tok.Literal)
}
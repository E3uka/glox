package main

import "strconv"

type Token uint

const (
	// Special Tokens (learned from go source): go/src/go/token/token.go
	ILLEGAL Token = iota
	EOF
	COMMENT

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

	EQL // ==
	LSS // <
	GTR // >
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

	EQL: "==",
	LSS: "<",
	GTR: ">",

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

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

package token

import (
	"fmt"
	"strconv"
)

type TokenType uint

const (
	// Special Tokens
	ILLEGAL TokenType = iota
	EOF                // added to end of source file for bookeeping

	// Literals
	IDENT  // main
	F64    // 123.45
	S64    // 12345
	STRING // "lets go"

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

	ADD     // +
	INCR    // ++
	INCRBY  // +=
	SUB     // -
	DECR    // --
	DECRYBY // -=
	STAR    // *
	QUO     // /

	NEQ // !=
	LSS // <
	LEQ // <=
	GTR // >
	GEQ // >=
	EQL // ==

	ASSIGN    // =
	WALRUS    // :=
	FUNASSIGN // ::
	FUNRETURN // ->
	CAST      // <-

	NOT // !
	BITAND // &
	AND    // &&
	BITOR  // |
	OR     // ||

	keyword_beg // Keywords
	CLASS
	STRUCT
	FALSE
	FOR
	BREAK

	IF
	CONST
	NULL

	RETURN
	SUPER
	IT
	TRUE
	WHILE

	BOOLTYPE   // bool
	F64TYPE    // f64
	S64TYPE    // s64
	STRINGTYPE // string
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	IDENT:  "IDENT",
	F64:    "FLOAT64",
	S64:    "SIGNED64",
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

	ADD:     "+",
	INCR:    "++",
	INCRBY:  "+=",
	SUB:     "-",
	DECR:    "--",
	DECRYBY: "-=",
	STAR:    "*",
	QUO:     "/",

	NEQ: "!=",
	LSS: "<",
	LEQ: "<=",
	GTR: ">",
	GEQ: ">=",
	EQL: "==",

	ASSIGN:    "=",
	WALRUS:    ":=",
	FUNASSIGN: "::",
	FUNRETURN: "->",
	CAST:      "<-",

	NOT:    "!",
	BITAND: "&",
	AND:    "&&",
	BITOR:  "|",
	OR:     "||",

	CLASS:  "class",
	STRUCT: "struct",
	FALSE:  "false",
	FOR:    "for",
	BREAK:  "break",

	IF:    "if",
	CONST: "const",
	NULL:  "NULL",

	RETURN: "return",
	SUPER:  "super",
	IT:     "it",
	TRUE:   "true",
	WHILE:  "while",

	BOOLTYPE:   "bool",
	F64TYPE:    "f64",
	S64TYPE:    "s64",
	STRINGTYPE: "string",
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
	Type    TokenType
	Literal string
	Start   int
	End     int
	Line    int
}

func (tok Token) String() string {
	return fmt.Sprintf("%s %s", tok.Type, tok.Literal)
}

// map with the exact size of number of keywords
var keywords = make(map[string]TokenType, keyword_end-(keyword_beg+1))

// Run on initialisation ~ gleaned from go source
func init() {
	// loop through tokens and add to map
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

func LookupKeyword(ident string) TokenType {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	return IDENT
}

package token

import (
	"fmt"
	"strconv"
)

type TOKEN_TYPE uint

const (
	// Special Tokens
	ILLEGAL TOKEN_TYPE = iota
	EOF                // added to end of source file for bookeeping

	// Literals
	IDENT  // main
	FLOAT  // 123.45
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
	MUL     // *
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

	IF
	CONST
	NULL
	PRINT

	RETURN
	SUPER
	IT
	TRUE
	WHILE
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",

	IDENT:  "IDENT",
	FLOAT:  "float",
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
	MUL:     "*",
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

	NOT: "!",
	BITAND: "&",
	AND:    "&&",
	BITOR:  "|",
	OR:     "||",

	CLASS:  "class",
	STRUCT: "struct",
	FALSE:  "false",
	FOR:    "for",

	IF:    "if",
	CONST: "const",
	NULL:  "null",
	PRINT: "print",

	RETURN: "return",
	SUPER:  "super",
	IT:     "it",
	TRUE:   "true",
	WHILE:  "while",
}

func (tt TOKEN_TYPE) String() string {
	s := ""
	if 0 <= tt && tt < TOKEN_TYPE(len(tokens)) {
		s = tokens[tt]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tt)) + ")"
	}
	return s
}

type Token struct {
	Type    TOKEN_TYPE
	Literal string
	Start   int
	End     int
	Line    int
}

func (tok Token) String() string {
	return fmt.Sprintf("%s %s", tok.Type, tok.Literal)
}

// map with the exact size of number of keywords
var keywords = make(map[string]TOKEN_TYPE, keyword_end-(keyword_beg+1))

// Run on initialisation ~ gleaned from go source
func init() {
	// loop through tokens and add to map
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

func Lookup_Keyword(ident string) TOKEN_TYPE {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	return IDENT
}

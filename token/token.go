package token

import (
	"fmt"
	"strconv"
)

type TOKEN_TYPE uint

const (
	// Special Tokens
	ILLEGAL TOKEN_TYPE = iota
	EOF                // automatically appened to end of source file.

	// Literals
	literal_beg // (trick learned from go source): go/src/go/token/token.go
	IDENT       // main
	FLOAT       // 123.45
	STRING      // "lets go"
	literal_end

	// Operators and delimiters
	operator_beg
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

	NOT // !
	NEQ // !=
	LEQ // <=
	GEQ // >=

	ASSIGN    // =
	FUNASSIGN // ::
	EQL       // ==
	LSS       // <
	GTR       // >
	FUNRET    // ->

	BITAND // &
	AND    // &&
	BITOR  // |
	OR     // ||
	operator_end

	keyword_beg // Keywords
	CLASS
	STRUCT
	FALSE
	FN
	FOR

	IF
	LET
	MUT
	NIL
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

	NOT: "!",
	NEQ: "!=",
	LEQ: "<=",
	GEQ: ">=",

	ASSIGN:    "=",
	FUNASSIGN: "::",
	EQL:       "==",
	LSS:       "<",
	GTR:       ">",
	FUNRET:    "->",

	BITAND: "&",
	AND:    "&&",
	BITOR:  "|",
	OR:     "||",

	CLASS:  "class",
	STRUCT: "struct",
	FALSE:  "false",
	FN:     "fn",
	FOR:    "for",

	IF:    "if",
	MUT:   "mut",
	LET:   "let",
	NIL:   "nil",
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
	Literal interface{}
	Line    int
}

func (tok Token) String() string {
	return fmt.Sprintf("%s %s", tok.Type, tok.Literal)
}

var keywords map[string]TOKEN_TYPE

// Run on initialisation ~ also gleaned from go source.
func init() {
	// make a map with the exact size of the length of keywords.
	keywords = make(map[string]TOKEN_TYPE, keyword_end-(keyword_beg+1))

	// loop through tokens and add to map
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

// Lookup maps an identifier to its keyword token or IDENT TOKEN_TYPE.
func Lookup(ident string) TOKEN_TYPE {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	return IDENT
}

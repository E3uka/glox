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
	CMNT               // // - /* */

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
	WALRUS    // :=
	MODASSIGN // ::
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
	DEFER // cool to implement
	STRUCT
	SOA // also cool to implement
	ELSE
	FALSE
	FN
	FOR

	IF
	LET
	NIL
	PRINT

	RETURN
	SUPER
	THIS
	IT
	ITERINDEX
	TRUE
	WHILE
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	CMNT:    "CMNT",

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
	WALRUS:    ":=",
	MODASSIGN: "::",
	EQL:       "==",
	LSS:       "<",
	GTR:       ">",
	FUNRET:    "->",

	BITAND: "&",
	AND:    "&&",
	BITOR:  "|",
	OR:     "||",

	CLASS:  "class",
	DEFER:  "defer",
	STRUCT: "struct",
	SOA:    "SOA",
	ELSE:   "else",
	FALSE:  "false",
	FN:     "fn",
	FOR:    "for",

	IF:    "if",
	LET:   "let",
	NIL:   "nil",
	PRINT: "print",

	RETURN:    "return",
	SUPER:     "super",
	THIS:      "this",
	IT:        "it",
	ITERINDEX: "it_index",
	TRUE:      "true",
	WHILE:     "while",
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

/*
TODO add information of line and character for both lexer and parser i.e.
lexer:

	l0 int
	c0 int

parser:

	l1 int
	c1 int
*/
type Token struct {
	Type    TOKEN_TYPE
	Lexeme  string
	Literal interface{}
	Line    int
}

func (tok Token) String() string {
	return fmt.Sprintf("%s %s %s", tok.Type, tok.Lexeme, tok.Literal)
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
// keywords)
func Lookup(ident string) TOKEN_TYPE {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	return IDENT
}

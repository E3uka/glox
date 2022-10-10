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

	NOT        // !
	NEQ        // !=
	OWNPOINTER // !*
	LEQ        // <=
	GEQ        // >=

	ASSIGN    // =
	WALRUS    // :=
	MODASSIGN // ::
	EQL       // ==
	LSS       // <
	GTR       // >

	BITAND  // &
	BITAND2 // &&
	BITOR   // &
	BITOR2  // &&

	operator_end

	keyword_beg
	// Keywords
	AND
	CLASS
	STRUCT
	ELSE
	FALSE
	FN
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
	VAR
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

	NOT:        "!",
	NEQ:        "!=",
	OWNPOINTER: "!*",
	LEQ:        "<=",
	GEQ:        ">=",

	ASSIGN:    "=",
	WALRUS:    ":=",
	MODASSIGN: "::",
	EQL:       "==",
	LSS:       "<",
	GTR:       ">",

	BITAND:  "&",
	BITAND2: "&&",
	BITOR:   "|",
	BITOR2:  "||",

	AND:    "and",
	CLASS:  "class",
	STRUCT: "struct",
	ELSE:   "else",
	FALSE:  "false",
	FN:     "fn",
	FOR:    "for",

	IF:    "if",
	LET:   "let",
	NIL:   "nil",
	OR:    "or",
	PRINT: "print",

	RETURN: "return",
	SUPER:  "super",
	THIS:   "this",
	TRUE:   "true",
	VAR:    "var",
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

var keywords map[string]TokenType

// Run on initialisation ~ taken from go source.
func init() {
	// make a map with the exact size of the length of keywords.
	keywords = make(map[string]TokenType, keyword_end-(keyword_beg+1))

	// loop through tokens and add to map
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

// Lookup maps an identifier to its keyword token or IDENT TokenType.
// keywords)
func Lookup(ident string) TokenType {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	return IDENT
}

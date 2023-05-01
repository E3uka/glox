package token

import (
	"strconv"
)

type TokenType uint16

const (
	// Special Tokens
	ILLEGAL TokenType = iota
	EOF               // added to end of source file for bookeeping

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

	NOT    // !
	BITAND // &
	AND    // &&
	BITOR  // |
	OR     // ||

	// Keywords
	keyword_beg
	CLASS
	STRUCT
	FALSE
	FOR
	BREAK
	TRAIT

	IF
	ELSE
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

var tokens = [...][]byte{
	ILLEGAL: []byte("ILLEGAL"),
	EOF:     []byte("EOF"),

	IDENT:  []byte("IDENT"),
	F64:    []byte("FLOAT64"),
	S64:    []byte("SIGNED64"),
	STRING: []byte("STRING"),

	LPAREN: []byte("("),
	LBRACK: []byte("["),
	LBRACE: []byte("{"),
	COMMA:  []byte(","),
	PERIOD: []byte("."),

	RPAREN:    []byte(")"),
	RBRACK:    []byte("]"),
	RBRACE:    []byte("}"),
	SEMICOLON: []byte(";"),
	COLON:     []byte(":"),

	ADD:     []byte("+"),
	INCR:    []byte("++"),
	INCRBY:  []byte("+="),
	SUB:     []byte("-"),
	DECR:    []byte("--"),
	DECRYBY: []byte("-="),
	STAR:    []byte("*"),
	QUO:     []byte("/"),

	NEQ: []byte("!="),
	LSS: []byte("<"),
	LEQ: []byte("<="),
	GTR: []byte(">"),
	GEQ: []byte(">="),
	EQL: []byte("=="),

	ASSIGN:    []byte("="),
	WALRUS:    []byte(":="),
	FUNASSIGN: []byte("::"),
	FUNRETURN: []byte("->"),
	CAST:      []byte("<-"),

	NOT:    []byte("!"),
	BITAND: []byte("&"),
	AND:    []byte("&&"),
	BITOR:  []byte("|"),
	OR:     []byte("||"),

	CLASS:  []byte("class"),
	STRUCT: []byte("struct"),
	FALSE:  []byte("false"),
	FOR:    []byte("for"),
	BREAK:  []byte("break"),
	TRAIT:  []byte("trait"),

	IF:    []byte("if"),
	ELSE:  []byte("else"),
	CONST: []byte("const"),
	NULL:  []byte("NULL"),

	RETURN: []byte("return"),
	SUPER:  []byte("super"),
	IT:     []byte("it"),
	TRUE:   []byte("true"),
	WHILE:  []byte("while"),

	BOOLTYPE:   []byte("bool"),
	F64TYPE:    []byte("f64"),
	S64TYPE:    []byte("s64"),
	STRINGTYPE: []byte("string"),
}

func (t TokenType) String() string {
	s := ""
	if t < TokenType(len(tokens)) {
		s = string(tokens[t])
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(t)) + ")"
	}
	return s
}

type Tokens struct {
	Tokens []TokenType
	Lit    [][]byte
	Line   []uint16
}

// map with the exact size of number of keywords
var keywords = make(map[string]TokenType, keyword_end-(keyword_beg+1))

// run on initialisation ~ gleaned from go source
func init() {
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[string(tokens[i])] = i
	}
}

func LookupKeyword(ident []byte) TokenType {
	if tok, is_keyword := keywords[string(ident)]; is_keyword {
		return tok
	}
	return IDENT
}

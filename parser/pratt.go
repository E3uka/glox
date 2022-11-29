package parser

import (
	"fmt"
	"glox/ast"
	"glox/token"
)

type pratt struct {
	path    *string
	tokens  []token.Token
	current int
	expr    ast.Expr
}

func (p *pratt) Expr() ast.Expr {
	return p.expr
}

func NewPratt(path *string, tokens *[]token.Token) *pratt {
	pratt := &pratt{
		path:    path,
		tokens:  *tokens,
		current: 0,
		expr:    nil,
	}

	pratt.init()
	return pratt
}

type precedence uint

const (
	LOWEST precedence = iota + 1
	EQUALITY
	LESSGREATER
	SUB
	ADD
	MUL
	QUO
	UNARY
)

type precedence_map = map[token.TokenType]precedence

var prec_map precedence_map

type null_denotation = map[token.TokenType]func(
	parser *pratt,
	item interface{},
) ast.Expr

type left_denotation = map[token.TokenType]func(
	parser *pratt,
	op token.TokenType,
	lhs ast.Expr,
) ast.Expr

var null_deno null_denotation
var left_deno left_denotation

func init() {
	prec_map = make(precedence_map)
	null_deno = make(null_denotation)
	left_deno = make(left_denotation)

	// populate maps
	prec_map[token.ADD] = ADD
	prec_map[token.SUB] = SUB
	prec_map[token.MUL] = MUL
	prec_map[token.QUO] = QUO
	prec_map[token.NOT] = UNARY

	null_deno[token.FLOAT] = nd_parse_float
	null_deno[token.SUB] = nd_parse_unary
	null_deno[token.NOT] = nd_parse_unary

	left_deno[token.ADD] = ld_parse_infix
	left_deno[token.SUB] = ld_parse_infix
	left_deno[token.MUL] = ld_parse_infix
	left_deno[token.QUO] = ld_parse_infix
}

func nd_parse_float(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token) // error elided; would not happen
	fmt.Printf("nd_parse_float: %v\n", token.Literal)
	return ast.LiteralExpr{Value: token.Literal}
}

func nd_parse_unary(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token) // error elided; would not happen
	right := parser.parse_expression(UNARY)
	fmt.Printf("nd_parse_unary: %v\n", token.Lexeme)
	return ast.UnaryExpr{Operator: token.TokType, Rhs: right}
}

func ld_parse_infix(parser *pratt, op token.TokenType, lhs ast.Expr) ast.Expr {
	parser.advance()
	op_prec := prec_map[parser.peek().TokType]
	right := parser.parse_expression(op_prec)
	fmt.Printf("ld_parse_infix\n")
	return ast.BinaryExpr{Lhs: lhs, Operator: op, Rhs: right}
}

func (p *pratt) init() {
	p.expr = p.parse_expression(LOWEST)
}

func (p *pratt) parse_expression(prec precedence) ast.Expr {
	var left ast.Expr
	var outer_tok token.Token
	var inner_tok token.Token

	outer_tok = p.peek()
	inner_tok = p.advance()

	left = null_deno[outer_tok.TokType](p, outer_tok)

	for !p.isAtEnd() && prec < prec_map[inner_tok.TokType] {
		outer_tok = inner_tok
		left = left_deno[outer_tok.TokType](p, outer_tok.TokType, left)
	}

	return left
}

func (p *pratt) advance() token.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.peek()
}

func (p *pratt) peek() token.Token {
	return p.tokens[p.current]
}

func (p *pratt) isAtEnd() bool {
	return p.peek().TokType == token.EOF
}

func (p *pratt) isAtStart() bool {
	// respects inital step in parse_expression method
	return p.current == 1
}

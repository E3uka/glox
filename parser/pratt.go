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
	SUM
	PRODUCT
)

type precedence_map = map[token.TokenType]precedence

var prec_map precedence_map

type null_denotation = map[token.TokenType]func(item interface{}) ast.Expr
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
	prec_map[token.ADD] = SUM
	prec_map[token.SUB] = SUM
	prec_map[token.MUL] = PRODUCT
	prec_map[token.QUO] = PRODUCT

	null_deno[token.ADD] = nd_terminate_early

	null_deno[token.FLOAT] = nd_parse_float

	left_deno[token.ADD] = ld_parse_infix
	left_deno[token.SUB] = ld_parse_infix
	left_deno[token.MUL] = ld_parse_infix
	left_deno[token.QUO] = ld_parse_infix
}

func nd_parse_float(item interface{}) ast.Expr {
	token, _ := item.(token.Token) // error elided; would not happen
	fmt.Printf("nd_parse_float: %v\n", token.Literal)
	return ast.LiteralExpr{Value: token.Literal}
}

func nd_terminate_early(item interface{}) ast.Expr {
	return nil
}

func ld_parse_infix(parser *pratt, op token.TokenType, lhs ast.Expr) ast.Expr {
	op_prec := prec_map[op]
	parser.advance()
	right := parser.parse_expression(op_prec)
	fmt.Printf("resolving binary expr\n")
	return ast.BinaryExpr{Lhs: lhs, Operator: op, Rhs: right}
}

func (p *pratt) init() {
	p.expr = p.parse_expression(LOWEST)
}

func (p *pratt) parse_expression(prec precedence) ast.Expr {
	fmt.Printf("current precedence: %v\n", prec)
	var tok token.Token
	var left ast.Expr

	tok = p.peek()
	fmt.Printf("current token: %v\n", tok.Literal)
	left = null_deno[tok.TokType](tok)
	tok = p.advance()

	for !p.isAtEnd() && prec < prec_map[tok.TokType] {
		fmt.Printf("inner current token: %v\n", tok.TokType)
		left = left_deno[tok.TokType](p, tok.TokType, left)
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

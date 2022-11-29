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
	PRIMARY
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
	prec_map[token.EQL] = EQUALITY
	prec_map[token.NEQ] = EQUALITY
	prec_map[token.GTR] = LESSGREATER
	prec_map[token.GEQ] = LESSGREATER
	prec_map[token.LSS] = LESSGREATER
	prec_map[token.LEQ] = LESSGREATER
	prec_map[token.ADD] = ADD
	prec_map[token.SUB] = SUB
	prec_map[token.MUL] = MUL
	prec_map[token.QUO] = QUO
	prec_map[token.NOT] = UNARY

	left_deno[token.EQL] = ld_parse_infix
	left_deno[token.NEQ] = ld_parse_infix
	left_deno[token.GTR] = ld_parse_infix
	left_deno[token.GEQ] = ld_parse_infix
	left_deno[token.LSS] = ld_parse_infix
	left_deno[token.LEQ] = ld_parse_infix
	left_deno[token.ADD] = ld_parse_infix
	left_deno[token.SUB] = ld_parse_infix
	left_deno[token.MUL] = ld_parse_infix
	left_deno[token.QUO] = ld_parse_infix

	null_deno[token.SUB] = nd_parse_unary
	null_deno[token.NOT] = nd_parse_unary

	null_deno[token.FLOAT] = nd_parse_float

	null_deno[token.STRING] = nd_parse_string

	null_deno[token.TRUE] = nd_parse_ident
	null_deno[token.FALSE] = nd_parse_ident
	null_deno[token.NIL] = nd_parse_ident

	null_deno[token.LPAREN] = nd_parse_group
}

func nd_parse_float(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token)
	fmt.Printf("parse_float: %v\n", token.Literal)
	return ast.LiteralExpr{Value: token.Literal}
}

func nd_parse_string(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token)
	fmt.Printf("parse_string: %v\n", token.Lexeme)
	return ast.LiteralExpr{Value: token.Lexeme}
}

func nd_parse_ident(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token)
	fmt.Printf("parse_bool: %v\n", token.TokType)
	return ast.LiteralExpr{Value: token.TokType}
}

func nd_parse_group(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token)
	fmt.Printf("parse_group: %v\n", token.TokType)
	expr := parser.parse_expression(LOWEST)
	parser.advance()
	return ast.GroupingExpr{Expression: expr}
}

func nd_parse_unary(parser *pratt, item interface{}) ast.Expr {
	token, _ := item.(token.Token)
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
	cur_tok := p.peek()
	p.advance()
	fmt.Printf("current_tok: %v, next_tok: %v\n", cur_tok.Lexeme, p.peek().Lexeme)
	left = null_deno[cur_tok.TokType](p, cur_tok)

	for !p.isAtEnd() && prec < prec_map[p.peek().TokType] {
		cur_tok = p.peek()
		left = left_deno[cur_tok.TokType](p, cur_tok.TokType, left)
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

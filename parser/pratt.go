package parser

import (
	"glox/ast"
	gloxError "glox/error"
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

func (p *pratt) init() {
	p.expr = p.parse_expression(LOWEST)
}

func (p *pratt) parse_expression(cur_prec precedence) ast.Expr {
	var left ast.Expr
	cur_tok := p.peek()
	// step past the first token then parse its subexpression
	p.advance()
	left = null_deno[cur_tok.TokType](p, cur_tok)
	for !p.isAtEnd() && cur_prec < prec_map[p.peek().TokType] {
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

type precedence uint

const (
	LOWEST precedence = iota
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
type null_denotation = map[token.TokenType]func(*pratt, token.Token) ast.Expr
type left_denotation = map[token.TokenType]func(
	*pratt,
	token.TokenType,
	ast.Expr,
) ast.Expr

var prec_map precedence_map
var null_deno null_denotation
var left_deno left_denotation

func init() {
	prec_map = make(precedence_map)
	null_deno = make(null_denotation)
	left_deno = make(left_denotation)

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

	left_deno[token.EQL] = ld_parse_binary
	left_deno[token.NEQ] = ld_parse_binary
	left_deno[token.GTR] = ld_parse_binary
	left_deno[token.GEQ] = ld_parse_binary
	left_deno[token.LSS] = ld_parse_binary
	left_deno[token.LEQ] = ld_parse_binary
	left_deno[token.ADD] = ld_parse_binary
	left_deno[token.SUB] = ld_parse_binary
	left_deno[token.MUL] = ld_parse_binary
	left_deno[token.QUO] = ld_parse_binary

	null_deno[token.SUB] = nd_parse_unary
	null_deno[token.NOT] = nd_parse_unary

	null_deno[token.FLOAT] = nd_parse_literal
	null_deno[token.STRING] = nd_parse_literal

	null_deno[token.TRUE] = nd_parse_identity
	null_deno[token.FALSE] = nd_parse_identity
	null_deno[token.NIL] = nd_parse_identity

	null_deno[token.LPAREN] = nd_parse_grouping
}

func nd_parse_literal(parser *pratt, tok token.Token) ast.Expr {
	return ast.LiteralExpr{Value: tok.Literal}
}

func nd_parse_identity(parser *pratt, tok token.Token) ast.Expr {
	return ast.LiteralExpr{Value: tok.TokType}
}

func nd_parse_grouping(parser *pratt, tok token.Token) ast.Expr {
	expr := parser.parse_expression(LOWEST)
	if parser.peek().TokType == token.EOF {
		gloxError.ParsePanic(parser.path, tok, "expected ')'")
	}
	// step past the closing ')'
	parser.advance()
	return ast.GroupingExpr{Expression: expr}
}

func nd_parse_unary(parser *pratt, tok token.Token) ast.Expr {
	expr := parser.parse_expression(UNARY)
	return ast.UnaryExpr{Operator: tok.TokType, Rhs: expr}
}

func ld_parse_binary(parser *pratt, op token.TokenType, lhs ast.Expr) ast.Expr {
	cur_prec := prec_map[parser.peek().TokType]
	// step past the infix operator then parse and capture the rhs subexpression
	// using the operators precedence
	parser.advance()
	expr := parser.parse_expression(cur_prec)
	return ast.BinaryExpr{Lhs: lhs, Operator: op, Rhs: expr}
}

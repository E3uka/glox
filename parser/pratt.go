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
	left = null_deno[cur_tok.Type](p, cur_tok)
	for !p.isAtEnd() && cur_prec < prec_map[p.peek().Type] {
		cur_tok = p.peek()
		left = left_deno[cur_tok.Type](p, cur_tok.Type, left)
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
	return p.peek().Type == token.EOF
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

var (
	prec_map  map[token.TOKEN_TYPE]precedence
	null_deno map[token.TOKEN_TYPE]func(*pratt, token.Token) ast.Expr
	left_deno map[token.TOKEN_TYPE]func(*pratt, token.TOKEN_TYPE, ast.Expr) ast.Expr
)

// init maps and populate with the appropriate parser dispatch functions
func init() {
	prec_map = make(map[token.TOKEN_TYPE]precedence)
	null_deno = make(map[token.TOKEN_TYPE]func(*pratt, token.Token) ast.Expr)
	left_deno = make(map[token.TOKEN_TYPE]func(*pratt, token.TOKEN_TYPE, ast.Expr) ast.Expr)

	prec_map[token.EQL] = EQUALITY
	prec_map[token.NEQ] = EQUALITY
	prec_map[token.GTR] = LESSGREATER
	prec_map[token.GEQ] = LESSGREATER
	prec_map[token.LSS] = LESSGREATER
	prec_map[token.LEQ] = LESSGREATER
	prec_map[token.ADD] = ADD
	prec_map[token.INCR] = UNARY
	prec_map[token.INCRBY] = ADD
	prec_map[token.SUB] = SUB
	prec_map[token.DECR] = UNARY
	prec_map[token.DECRYBY] = SUB
	prec_map[token.MUL] = MUL
	prec_map[token.QUO] = QUO

	null_deno[token.FLOAT] = nd_parse_literal
	null_deno[token.STRING] = nd_parse_literal

	null_deno[token.SUB] = nd_parse_unary
	null_deno[token.NOT] = nd_parse_unary

	null_deno[token.TRUE] = nd_parse_identity
	null_deno[token.FALSE] = nd_parse_identity
	null_deno[token.NIL] = nd_parse_identity

	null_deno[token.LPAREN] = nd_parse_grouping

	left_deno[token.INCR] = ld_parse_unary
	left_deno[token.DECR] = ld_parse_unary

	left_deno[token.EQL] = ld_parse_binary
	left_deno[token.NEQ] = ld_parse_binary
	left_deno[token.GTR] = ld_parse_binary
	left_deno[token.GEQ] = ld_parse_binary
	left_deno[token.LSS] = ld_parse_binary
	left_deno[token.LEQ] = ld_parse_binary
	left_deno[token.ADD] = ld_parse_binary
	left_deno[token.INCRBY] = ld_parse_binary
	left_deno[token.SUB] = ld_parse_binary
	left_deno[token.DECRYBY] = ld_parse_binary
	left_deno[token.MUL] = ld_parse_binary
	left_deno[token.QUO] = ld_parse_binary

}

func nd_parse_literal(parser *pratt, tok token.Token) ast.Expr {
	return ast.LiteralExpr{Value: tok.Literal}
}

func nd_parse_identity(parser *pratt, tok token.Token) ast.Expr {
	return ast.LiteralExpr{Value: tok.Type}
}

func nd_parse_grouping(parser *pratt, tok token.Token) ast.Expr {
	expr := parser.parse_expression(LOWEST)
	if parser.peek().Type == token.EOF {
		gloxError.ParsePanic(parser.path, tok, "expected ')'")
	}
	// step past the closing ')'
	parser.advance()
	return ast.GroupingExpr{Expression: expr}
}

func nd_parse_unary(parser *pratt, tok token.Token) ast.Expr {
	expr := parser.parse_expression(UNARY)
	return ast.UnaryExpr{Operator: tok.Type, Rhs: expr}
}

func ld_parse_unary(
	parser *pratt,
	op token.TOKEN_TYPE,
	lhs ast.Expr,
) ast.Expr {
	// step past the postfix operator
	parser.advance()
	return ast.UnaryExpr{Operator: op, Rhs: lhs}
}

func ld_parse_binary(
	parser *pratt,
	op token.TOKEN_TYPE,
	lhs ast.Expr,
) ast.Expr {
	op_prec := prec_map[parser.peek().Type]
	// step past the infix operator
	parser.advance()
	expr := parser.parse_expression(op_prec)
	return ast.BinaryExpr{Lhs: lhs, Operator: op, Rhs: expr}
}

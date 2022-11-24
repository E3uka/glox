package parser

import (
	"glox/ast"
	gloxError "glox/error"
	"glox/token"
)

type parser struct {
	tokens  []token.Token
	current int
}

func New(tokens []token.Token) parser {
	return parser{
		tokens:  tokens,
		current: 0,
	}
}

func (p *parser) Parse() ast.Expr {
	defer func() {
		if r := recover(); r != nil {
			return
		}
	}()
	return p.expression()
}

func (p *parser) expression() ast.Expr {
	return p.equality()
}

func (p *parser) equality() ast.Expr {
	expr := p.comparison()
	for p.match(token.NEQ, token.EQL) {
		operator := p.previous()
		right := p.comparison()
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr
}

func (p *parser) comparison() ast.Expr {
	expr := p.term()
	for p.match(token.GTR, token.GEQ, token.LSS, token.LEQ) {
		operator := p.previous()
		right := p.term()
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr
}

func (p *parser) term() ast.Expr {
	expr := p.factor()
	for p.match(token.ADD, token.SUB) {
		operator := p.previous()
		right := p.factor()
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr
}

func (p *parser) factor() ast.Expr {
	expr := p.unary()
	for p.match(token.QUO, token.MUL) {
		operator := p.previous()
		right := p.unary()
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr
}

func (p *parser) unary() ast.Expr {
	if p.match(token.NOT, token.SUB) {
		operator := p.previous()
		right := p.unary()
		return ast.UnaryExpr{
			Operator: operator,
			Rhs:      right,
		}
	}
	return p.primary()
}

func (p *parser) primary() ast.Expr {
	if p.match(token.FALSE) {
		return ast.LiteralExpr{Value: false}
	}
	if p.match(token.TRUE) {
		return ast.LiteralExpr{Value: true}
	}
	if p.match(token.NIL) {
		return ast.LiteralExpr{Value: nil}
	}

	if p.match(token.FLOAT, token.STRING) {
		return ast.LiteralExpr{Value: p.previous().Literal}
	}

	if p.match(token.LPAREN) {
		expr := p.expression()
		p.consume(token.RPAREN, "Expected ')' after expression.")
		return ast.GroupingExpr{Expression: expr}
	}

	p.reportGloxError(p.peek(), "Expected expression")
	panic(struct{}{}) // signal;
}

func (p *parser) match(tokTypes ...token.TokenType) bool {
	for _, tt := range tokTypes {
		if p.check(tt) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *parser) consume(tokType token.TokenType, msg string) token.Token {
	if p.check(tokType) {
		return p.advance()
	}
	panic(p.peek())
}

func (p *parser) synchronize() {
	p.advance()
	for !p.isAtEnd() {
		if p.previous().TokType == token.SEMICOLON {
			return
		}

		switch p.peek().TokType {
		case token.CLASS, token.FOR, token.FN, token.IF, token.PRINT,
			token.RETURN, token.LET, token.WHILE:
			return
		}
		p.advance()
	}
}

func (p *parser) check(tokType token.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().TokType == tokType
}

func (p *parser) advance() token.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *parser) peek() token.Token {
	return p.tokens[p.current]
}

func (p *parser) previous() token.Token {
	return p.tokens[p.current-1]
}

func (p *parser) reportGloxError(tok token.Token, msg string) {
	gloxError.ReportParserError(tok, msg)
}

func (p *parser) isAtEnd() bool {
	return p.peek().TokType == token.EOF
}

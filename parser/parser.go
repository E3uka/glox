package parser

import (
	"fmt"
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
			// Do nothing if an error occurs
			return
		}
	}()
	return p.expression()
}

func (p *parser) expression() ast.Expr {
	return p.equality()
}

func (p *parser) equality() ast.Expr {
	fmt.Println("equality")
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
	fmt.Println("comparison")
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
	fmt.Println("term")
	expr := p.factor()
	for p.match(token.SUB, token.ADD) {
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

func (p *parser) factor() ast.Expr {
	fmt.Println("factor")
	expr := p.unary()
	for p.match(token.QUO, token.MUL) {
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

func (p *parser) unary() ast.Expr {
	fmt.Println("unary")
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
	fmt.Println("primary")
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
		fmt.Println("potential error on l paren")
		expr := p.expression()
		p.consume(token.RPAREN, "Expected ')' after expression.")
		return ast.GroupingExpr{Expression: expr}
	}

	gloxError.ReportParserError(p.peek(), "Expected expression")
	panic("expression parse error")
}

func (p *parser) match(tokTypes ...token.TokenType) bool {
	for _, tok := range tokTypes {
		if p.check(tok) {
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
	if p.isAtEnd() {
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

func (p *parser) error(tok token.Token, msg string) {
	gloxError.ReportParserError(tok, msg)
}

func (p *parser) isAtEnd() bool {
	return p.current >= len(p.tokens)
}

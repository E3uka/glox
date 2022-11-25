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

func (p *parser) Parse() (ast.Expr, error) {
	if expr, err := p.expression(); err != nil {
		return nil, err
	} else {
		return expr, nil
	}
}

func (p *parser) expression() (ast.Expr, error) {
	if expr, err := p.equality(); err != nil {
		return nil, err
	} else {
		return expr, nil
	}
}

func (p *parser) equality() (ast.Expr, error) {
	var expr ast.Expr
	var err error
	expr, err = p.comparison()
	if err != nil {
		return nil, err
	}
	for p.match(token.NEQ, token.EQL) {
		operator := p.previous()
		right, err := p.comparison()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr, nil
}

func (p *parser) comparison() (ast.Expr, error) {
	var expr ast.Expr
	var err error
	expr, err = p.term()
	if err != nil {
		return nil, err
	}
	for p.match(token.GTR, token.GEQ, token.LSS, token.LEQ) {
		operator := p.previous()
		right, err := p.term()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr, nil
}

func (p *parser) term() (ast.Expr, error) {
	var expr ast.Expr
	var err error
	expr, err = p.factor()
	if err != nil {
		return nil, err
	}
	for p.match(token.ADD, token.SUB) {
		operator := p.previous()
		right, err := p.factor()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr, nil
}

func (p *parser) factor() (ast.Expr, error) {
	var expr ast.Expr
	var err error
	expr, err = p.unary()
	if err != nil {
		return nil, err
	}
	for p.match(token.QUO, token.MUL) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator,
			Rhs:      right,
		}
	}
	return expr, nil
}

func (p *parser) unary() (ast.Expr, error) {
	if p.match(token.NOT, token.SUB) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		return ast.UnaryExpr{Operator: operator, Rhs: right}, nil
	}
	return p.primary()
}

func (p *parser) primary() (ast.Expr, error) {
	var err error
	var expr ast.Expr
	if p.match(token.FALSE) {
		return ast.LiteralExpr{Value: false}, nil
	}
	if p.match(token.TRUE) {
		return ast.LiteralExpr{Value: true}, nil
	}
	if p.match(token.NIL) {
		return ast.LiteralExpr{Value: nil}, nil
	}

	if p.match(token.FLOAT, token.STRING) {
		return ast.LiteralExpr{Value: p.previous().Literal}, nil
	}

	if p.match(token.LPAREN) {
		expr, err = p.expression()
		if err != nil {
			return nil, err
		}
		err = p.consume(
			token.Token{
				TokType: token.RPAREN,
				Lexeme:  token.RPAREN.String(),
				Line:    p.current,
			},
			"Expected ')' after expression.",
		)
		if err != nil {
			return nil, err
		}
		return ast.GroupingExpr{Expression: expr}, nil
	}
	return nil, gloxError.ParseError(
		token.Token{
			TokType: p.peek().TokType,
			Lexeme:  p.peek().Lexeme,
			Literal: p.peek().Literal,
			Line:    p.current,
		},
		"unrecognised token",
	)
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

func (p *parser) consume(token token.Token, msg string) error {
	if p.check(token.TokType) {
		p.advance()
		return nil
	}
	return gloxError.ParseError(token, msg)
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

func (p *parser) isAtEnd() bool {
	return p.peek().TokType == token.EOF
}

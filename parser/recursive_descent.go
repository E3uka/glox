package parser

import (
	"errors"
	"glox/ast"
	"glox/token"
)

type parser struct {
	path    *string
	tokens  []token.Token
	current int
	expr    ast.Expr
}

func NewRdParser(path *string, tokens *[]token.Token) (*parser, error) {
	parser := &parser{
		path:    path,
		tokens:  *tokens,
		current: 0,
		expr:    nil,
	}

	if err := parser.init(); err != nil {
		return nil, err
	}
	return parser, nil
}

func (p *parser) Expr() *ast.Expr {
	return &p.expr
}

func (p *parser) init() error {
	expr, err := p.expression()
	if err != nil {
		return err
	}
	p.expr = expr
	return nil
}

func (p *parser) expression() (ast.Expr, error) {
	expr, err := p.equality()
	return expr, err
}

func (p *parser) equality() (ast.Expr, error) {
	expr, err := p.comparison()
	for p.match(token.NEQ, token.EQL) {
		operator := p.previous()
		right, err := p.comparison()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator.Type,
			Rhs:      right,
		}
	}
	return expr, err
}

func (p *parser) comparison() (ast.Expr, error) {
	expr, err := p.term()
	for p.match(token.GTR, token.GEQ, token.LSS, token.LEQ) {
		operator := p.previous()
		right, err := p.term()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator.Type,
			Rhs:      right,
		}
	}
	return expr, err
}

func (p *parser) term() (ast.Expr, error) {
	expr, err := p.factor()
	for p.match(token.SUB, token.ADD) {
		operator := p.previous()
		right, err := p.factor()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator.Type,
			Rhs:      right,
		}
	}
	return expr, err
}

func (p *parser) factor() (ast.Expr, error) {
	expr, err := p.unary()
	for p.match(token.QUO, token.MUL) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		expr = ast.BinaryExpr{
			Lhs:      expr,
			Operator: operator.Type,
			Rhs:      right,
		}
	}
	return expr, err
}

func (p *parser) unary() (ast.Expr, error) {
	if p.match(token.NOT, token.SUB) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		return ast.UnaryExpr{Operator: operator.Type, Rhs: right}, nil
	}
	return p.primary()
}

func (p *parser) primary() (ast.Expr, error) {
	if p.match(token.FALSE) {
		return ast.LiteralExpr{Value: false}, nil
	}
	if p.match(token.TRUE) {
		return ast.LiteralExpr{Value: true}, nil
	}
	if p.match(token.NULL) {
		return ast.LiteralExpr{Value: nil}, nil
	}
	if p.match(token.FLOAT, token.STRING) {
		return ast.LiteralExpr{Value: p.previous().Literal}, nil
	}

	if p.match(token.LPAREN) {
		expr, err := p.expression()
		// must explicitly handle the error below to prevent the consume call
		// from hiding the error
		if err != nil {
			return nil, err
		}
		err = p.consume(
			token.Token{
				Type:    token.RPAREN,
				Literal: struct{}{},
				Line:    p.current,
			},
			"expected ')' after expression.",
		)
		return ast.GroupingExpr{Expression: expr}, err
	}

	return nil, errors.New("unrecognised token")
}

func (p *parser) match(tok_types ...token.TOKEN_TYPE) bool {
	for _, tt := range tok_types {
		if p.check(tt) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *parser) consume(token token.Token, msg string) error {
	if p.check(token.Type) {
		p.advance()
		return nil
	}
	return errors.New("unrecognised token")
}

func (p *parser) synchronize() {
	p.advance()
	for !p.is_at_end() {
		if p.previous().Type == token.SEMICOLON {
			return
		}
		switch p.peek().Type {
		case token.CLASS, token.FOR, token.FN, token.IF, token.PRINT,
			token.RETURN, token.CONST, token.WHILE:
			return
		}
		p.advance()
	}
}

func (p *parser) check(tokType token.TOKEN_TYPE) bool {
	if p.is_at_end() {
		return false
	}
	return p.peek().Type == tokType
}

func (p *parser) advance() token.Token {
	if !p.is_at_end() {
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

func (p *parser) is_at_end() bool {
	return p.peek().Type == token.EOF
}

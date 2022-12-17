package parser

import (
	"fmt"
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

func New(path *string, tokens *[]token.Token) *pratt {
	pratt := &pratt{
		path:    path,
		tokens:  *tokens,
		current: 0,
		expr:    nil,
	}
	return pratt
}

func (p *pratt) Parse() []ast.StatementExpr {
	var statements []ast.StatementExpr
	for !p.is_at_end() {
		// wrapped in anonymous function so block is called as unit ~ enables
		// panic recovery to occur mutiple times instead of short-circuiting on
		// first capture; recovery advances to start of next viable statement
		// token which continues the parse operation from that point
		func() {
			defer func() {
				if r := recover(); r != nil {
					gloxError.Parse_Panic_Recover(fmt.Sprint(r))
					recover_and_sync(p)
				}
			}()
			// may panic
			stmt := parse_statements(p, p.peek())
			statements = append(statements, stmt)
		}()
	}
	return statements
}

func parse_statements(parser *pratt, tok token.Token) ast.StatementExpr {
	var identifier interface{}
	mutable := true
	reassignment := false
	// handle both ':=' and reassignment
	if parser.peek().Type != token.CONST {
		identifier = parser.peek().Literal
		parser.advance()
		if parser.peek().Type == token.ASSIGN {
			mutable = false
			reassignment = true
		}
	} else {
		mutable = false
		reassignment = false
		// step past 'const' keyword to identifier
		parser.advance()
		identifier = parser.peek().Literal
		// step past the identifier
		parser.advance()
	}

	if parser.peek().Type == token.SEMICOLON {
		if !mutable {
			parser.backtrack()
			gloxError.Parse_Panic(
				parser.path,
				parser.peek(),
				"cannot initialize an immutable null constant",
			)
		}
		// step past ';'
		parser.advance()
		return ast.StatementExpr{
			Ident:        identifier,
			Rhs:          ast.LiteralExpr{Value: nil},
			Mutable:      true,
			Reassignment: false,
		}
	}

	// step past walrus or assignment operator and begin recursive parse
	// operation with the lowest precedence
	parser.advance()
	expr := parser.parse_expression(LOWEST)
	if parser.is_at_end() {
		parser.backtrack()
		gloxError.Parse_Panic(parser.path, tok, "expected ';'")
	}
	// step past ';'
	parser.advance()
	return ast.StatementExpr{
		Ident:        identifier,
		Rhs:          expr,
		Mutable:      mutable,
		Reassignment: reassignment,
	}
}

func recover_and_sync(parser *pratt) {
	parser.advance()
	for !parser.is_at_end() {
		// found end statement boundary; advance to next token and continue the
		// parse operation
		if parser.peek().Type == token.SEMICOLON {
			parser.advance()
			return
		}
		switch parser.peek().Type {
		case token.CLASS, token.STRUCT, token.FOR, token.IF, token.CONST,
			token.RETURN, token.WHILE, token.FUNASSIGN:
			return
		}
		// advance until boundary is found
		parser.advance()
	}
}

// Top Down Operator Precedence - Vaughan R. Pratt, 1973
// https://dl.acm.org/doi/10.1145/512927.512931
func (p *pratt) parse_expression(current_precedence precedence) ast.Expr {
	var left ast.Expr
	cur_tok := p.peek()
	// step past the first token then parse its subexpression
	p.advance()
	left = null_deno[cur_tok.Type](p, cur_tok)
	// recursively parse and re-assign the top level expresssion
	for !p.is_at_end() && current_precedence < prec_map[p.peek().Type] {
		cur_tok = p.peek()
		left = left_deno[cur_tok.Type](p, cur_tok.Type, left)
	}
	return left
}

func (p *pratt) advance() token.Token { p.current++; return p.peek() }
func (p *pratt) backtrack()           { p.current-- }
func (p *pratt) peek() token.Token    { return p.tokens[p.current] }
func (p *pratt) is_at_end() bool      { return p.peek().Type == token.EOF }

type precedence uint

const (
	// why go doesn't have enums escapes me
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
	// token type -> operator precedence (binding power)
	prec_map = map[token.TOKEN_TYPE]precedence{}

	// token type -> null denotation; no expression found left of current token
	null_deno = map[token.TOKEN_TYPE]func(
		parser *pratt,
		cur_tok token.Token,
	) ast.Expr{}

	// token type -> left denotation; expression found left of current token
	left_deno = map[token.TOKEN_TYPE]func(parser *pratt,
		cur_tok token.TOKEN_TYPE,
		lhs ast.Expr,
	) ast.Expr{}
)

func init() {
	prec_map[token.ASSIGN] = LOWEST
	prec_map[token.WALRUS] = LOWEST
	prec_map[token.CONST] = LOWEST
	prec_map[token.EQL] = EQUALITY
	prec_map[token.NEQ] = EQUALITY
	prec_map[token.GEQ] = LESSGREATER
	prec_map[token.GTR] = LESSGREATER
	prec_map[token.LEQ] = LESSGREATER
	prec_map[token.LSS] = LESSGREATER
	prec_map[token.DECRYBY] = SUB
	prec_map[token.SUB] = SUB
	prec_map[token.ADD] = ADD
	prec_map[token.INCRBY] = ADD
	prec_map[token.MUL] = MUL
	prec_map[token.QUO] = QUO
	prec_map[token.DECR] = UNARY
	prec_map[token.INCR] = UNARY
	prec_map[token.IDENT] = PRIMARY

	null_deno[token.FALSE] = nd_parse_literal
	null_deno[token.FLOAT] = nd_parse_literal
	null_deno[token.IDENT] = nd_parse_ident

	null_deno[token.LBRACE] = nd_parse_block
	null_deno[token.LPAREN] = nd_parse_grouping
	null_deno[token.NOT] = nd_parse_unary
	null_deno[token.NULL] = nd_parse_literal
	null_deno[token.STRING] = nd_parse_literal
	null_deno[token.SUB] = nd_parse_unary
	null_deno[token.TRUE] = nd_parse_literal

	left_deno[token.ADD] = ld_parse_binary
	left_deno[token.DECRYBY] = ld_parse_binary
	left_deno[token.DECR] = ld_parse_unary
	left_deno[token.EQL] = ld_parse_binary
	left_deno[token.GEQ] = ld_parse_binary
	left_deno[token.GTR] = ld_parse_binary
	left_deno[token.INCRBY] = ld_parse_binary
	left_deno[token.INCR] = ld_parse_unary
	left_deno[token.LEQ] = ld_parse_binary
	left_deno[token.LSS] = ld_parse_binary
	left_deno[token.MUL] = ld_parse_binary
	left_deno[token.NEQ] = ld_parse_binary
	left_deno[token.QUO] = ld_parse_binary
	left_deno[token.SUB] = ld_parse_binary
}

func nd_parse_block(parser *pratt, tok token.Token) ast.Expr {
	// empty block scope '{}'
	if parser.peek().Type == token.RBRACE {
		parser.advance()
		return ast.GroupingExpr{
			Expression: ast.LiteralExpr{Value: nil},
		}
	}
	expr := parser.parse_expression(prec_map[tok.Type])
	if parser.peek().Type != token.RBRACE {
		parser.backtrack()
		gloxError.Parse_Panic(parser.path, tok, "expected '}'")
	}
	// step past the closing '}'
	parser.advance()
	return ast.GroupingExpr{Expression: expr}
}

func nd_parse_grouping(parser *pratt, tok token.Token) ast.Expr {
	// TODO: future: handle function scope 'func_name :: (arg1, arg2) { ... }'
	expr := parser.parse_expression(prec_map[tok.Type])
	if parser.peek().Type != token.RPAREN {
		parser.backtrack()
		gloxError.Parse_Panic(parser.path, tok, "expected ')'")
	}
	// step past the closing ')'
	parser.advance()
	return ast.GroupingExpr{Expression: expr}
}

func nd_parse_ident(parser *pratt, tok token.Token) ast.Expr {
	return ast.VariableExpr{Ident: ast.LiteralExpr{Value: tok.Literal}}
}

func nd_parse_literal(parser *pratt, tok token.Token) ast.Expr {
	return ast.LiteralExpr{Value: tok.Literal}
}

func nd_parse_unary(parser *pratt, tok token.Token) ast.Expr {
	expr := parser.parse_expression(prec_map[tok.Type])
	return ast.UnaryExpr{Operator: tok.Type, Rhs: expr}
}

func ld_parse_binary(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Expr,
) ast.Expr {
	// step past the infix operator
	parser.advance()
	expr := parser.parse_expression(prec_map[operator])
	return ast.BinaryExpr{Lhs: lhs, Operator: operator, Rhs: expr}
}

func ld_parse_unary(parser *pratt, op token.TOKEN_TYPE, lhs ast.Expr) ast.Expr {
	// step past the postfix operator
	parser.advance()
	return ast.UnaryExpr{Operator: op, Rhs: lhs}
}

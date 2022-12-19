package parser

import (
	"fmt"
	"glox/ast"
	glox_err "glox/error"
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

func (p *pratt) Parse() []ast.Node {
	var nodes []ast.Node
	for !p.is_at_end() {
		// wrapped in anonymous function so block is called as unit ~ enables
		// panic recovery to occur mutiple times instead of short-circuiting on
		// first capture; recovery advances to start of next viable statement
		// token which continues the parse operation from that point
		func() {
			defer func() {
				if r := recover(); r != nil {
					glox_err.Parse_Panic_Recover(fmt.Sprint(r))
					recover_and_sync(p)
				}
			}()
			// may panic - using LOWEST-1 precedence to ensure any found nodes
			// have a higher precedence
			// operation, all Nodes found will be of higher precedence
			node := p.parse_expression(LOWEST - 1)
			nodes = append(nodes, node)
		}()
	}
	return nodes
}

/*
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
*/

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
func (p *pratt) parse_expression(current_precedence precedence) ast.Node {
	var left ast.Node
	cur_tok := p.peek()
	// step past the first token then parse its subexpression
	p.advance()
	left = null_deno[cur_tok.Type](p, cur_tok)
	// recursively parse and re-assign the top level expresssion if it has a
	// higher binding power
	for !p.is_at_end() && current_precedence < prec_map[p.peek().Type] {
		cur_tok = p.peek()
		// step past semicolon as that shouldn't be used for any operation
		if cur_tok.Type == token.SEMICOLON {
			p.advance()
			break
		}
		left = left_deno[cur_tok.Type](p, cur_tok.Type, left)
	}
	return left
}

func (p *pratt) advance() token.Token { p.current++; return p.peek() }
func (p *pratt) backtrack()           { p.current-- }
func (p *pratt) peek() token.Token    { return p.tokens[p.current] }
func (p *pratt) is_at_end() bool      { return p.peek().Type == token.EOF }

type precedence int

const (
	// why go doesn't have proper enums escapes me
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
	) ast.Node{}

	// token type -> left denotation; expression found left of current token
	left_deno = map[token.TOKEN_TYPE]func(parser *pratt,
		cur_tok token.TOKEN_TYPE,
		lhs ast.Node,
	) ast.Node{}
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
	null_deno[token.LPAREN] = nd_parse_paren
	null_deno[token.NOT] = nd_parse_unary
	null_deno[token.NULL] = nd_parse_literal
	null_deno[token.STRING] = nd_parse_literal
	null_deno[token.SUB] = nd_parse_unary
	null_deno[token.TRUE] = nd_parse_literal

	// left denotation expressions
	left_deno[token.ADD] = ld_parse_binary_expr
	left_deno[token.DECRYBY] = ld_parse_binary_expr
	left_deno[token.DECR] = ld_parse_unary_expr
	left_deno[token.EQL] = ld_parse_binary_expr
	left_deno[token.GEQ] = ld_parse_binary_expr
	left_deno[token.GTR] = ld_parse_binary_expr
	left_deno[token.INCRBY] = ld_parse_binary_expr
	left_deno[token.INCR] = ld_parse_unary_expr
	left_deno[token.LEQ] = ld_parse_binary_expr
	left_deno[token.LSS] = ld_parse_binary_expr
	left_deno[token.MUL] = ld_parse_binary_expr
	left_deno[token.NEQ] = ld_parse_binary_expr
	left_deno[token.QUO] = ld_parse_binary_expr
	left_deno[token.SUB] = ld_parse_binary_expr

	// left denotation statements
	left_deno[token.ASSIGN] = ld_parse_assign_stmt // 'x = abc'
	left_deno[token.WALRUS] = ld_parse_decl_stmt   // 'x := abc'
}

func nd_parse_block(parser *pratt, tok token.Token) ast.Node {
	return nil
}

func nd_parse_paren(parser *pratt, tok token.Token) ast.Node {
	fmt.Println("nd_paren")
	// handle function scope
	expr := parser.parse_expression(prec_map[tok.Type])
	if parser.peek().Type != token.RPAREN {
		parser.backtrack()
		glox_err.Parse_Panic(parser.path, tok, "expected ')'")
	}
	// step past ')'
	parser.advance()
	return &ast.ParenExpr{Expr: expr.(ast.Expr)}
}

func nd_parse_ident(parser *pratt, tok token.Token) ast.Node {
	fmt.Printf("nd_indent: name: %v\n", tok.Literal)
	return ast.IdentExpr{
		Name: tok.Literal,
		Obj:  &ast.Object{Kind: ast.Var, Name: tok.Literal},
	}
}

func nd_parse_literal(parser *pratt, tok token.Token) ast.Node {
	fmt.Printf("nd_literal: kind: %v, value: %v\n", tok.Type, tok.Literal)
	return &ast.LiteralExpr{Kind: tok.Type, Value: tok.Literal}
}

func nd_parse_unary(parser *pratt, tok token.Token) ast.Node {
	fmt.Printf("nd_unary: operator %v\n", tok)
	parser.advance()
	expr := parser.parse_expression(prec_map[parser.peek().Type])
	fmt.Println("you can make it")
	return &ast.UnaryExpr{Operator: tok.Type, Rhs: expr.(ast.Expr)}
}

func ld_parse_binary_expr(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	fmt.Println("ld_binary")
	// step past infix operator
	parser.advance()
	expr := parser.parse_expression(prec_map[parser.peek().Type])
	return &ast.BinaryExpr{
		Lhs:      lhs.(ast.Expr),
		Operator: operator,
		Rhs:      expr.(ast.Expr),
	}
}

func ld_parse_unary_expr(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	fmt.Println("ld_unary")
	// step past postfix operator
	parser.advance()
	return &ast.UnaryExpr{Operator: operator, Rhs: lhs.(ast.Expr)}
}

func ld_parse_assign_stmt(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	fmt.Println("ld_assign_stmt")
	// step past assign operator
	parser.advance()
	rhs := parser.parse_expression(prec_map[parser.peek().Type])
	return &ast.AssignStmt{
		Lhs:   []ast.Expr{lhs.(ast.Expr)},
		Token: operator,
		Rhs:   []ast.Expr{rhs.(ast.Expr)},
	}
}

func ld_parse_decl_stmt(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	fmt.Printf("ld_decl_stmt: operator: %v\n", operator)
	// step past walrus operator
	parser.advance()
	left_as_ident := lhs.(ast.IdentExpr)
	rhs := parser.parse_expression(prec_map[parser.peek().Type])

	return &ast.DeclStmt{
		Decl: &ast.GenericDecl{
			Name: ast.IdentExpr{
				Name: left_as_ident.Name,
				Obj:  left_as_ident.Obj,
			},
			Tok:   operator,
			Value: rhs.(ast.Expr),
		},
	}
}

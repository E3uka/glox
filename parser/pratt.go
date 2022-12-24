package parser

import (
	"fmt"
	"glox/ast"
	glox_err "glox/error"
	"glox/token"
	"reflect"
)

type pratt struct {
	path    *string
	tokens  []token.Token
	current int
	expr    ast.Expr
	trace   bool
}

func New(path *string, tokens *[]token.Token) *pratt {
	pratt := &pratt{
		path:    path,
		tokens:  *tokens,
		current: 0,
		expr:    nil,
		trace:   true,
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
			// may panic - using an initial 'LOWEST - 1' precedence to ensure
			// all latter found Nodes will be of higher precedence and thus 
			// accepted as valid nodes due to having a higher binding power
			node := p.parse_node(LOWEST - 1)
			nodes = append(nodes, node)
			if p.trace {
				fmt.Printf("------ PARSED: [%v] ------\n", reflect.TypeOf(node))
			}
		}()
	}
	return nodes
}

func recover_and_sync(parser *pratt) {
	next_tok := parser.advance()
	for !parser.is_at_end() {
		// step past end node boundary ';' to continue parse operation
		if next_tok.Type == token.SEMICOLON {
			parser.expect(token.SEMICOLON)
			return
		}
		// potential beginning statement boundary (if formatted correctly)
		switch parser.peek().Type {
		case token.CLASS, token.STRUCT, token.FOR, token.IF, token.CONST,
			token.RETURN, token.WHILE, token.FUNASSIGN:
			return
		}
		// advance until boundary is found
		next_tok = parser.advance()
	}
}

// Top Down Operator Precedence - Vaughan R. Pratt, 1973
// https://dl.acm.org/doi/10.1145/512927.512931
func (p *pratt) parse_node(current_precedence precedence) ast.Node {
	var left ast.Node
	cur_tok := p.peek()
	// step past the first token and parse its subexpression
	p.advance()
	if p.trace {
		fmt.Printf(
			"parse_head: cur_pred %v, cur_tok: %v, next_tok: %v, calc_pred: %v\n",
			current_precedence,
			cur_tok,
			p.peek(),
			prec_map[cur_tok.Type],
		)
	}
	left = null_deno[cur_tok.Type](p, cur_tok)
	// recursively parse and re-assign both current token (used for future
	// precedence calculations) and the top level expresssion but only if the
	// subexpression has a higher precedence (binding power)
	for !p.is_at_end() && current_precedence < prec_map[p.peek().Type] {
		cur_tok = p.peek()
		// step past end node boundary ';' to next token boundary and break
		// from recursive loop
		if cur_tok.Type == token.SEMICOLON {
			p.advance()
			break
		}
		left = left_deno[cur_tok.Type](p, cur_tok.Type, left)
	}
	return left
}

func (p *pratt) advance() token.Token { p.current++; return p.peek() }
func (p *pratt) peek() token.Token    { return p.tokens[p.current] }
func (p *pratt) is_at_end() bool      { return p.peek().Type == token.EOF }
// expect is like advance but raises are parser error if the next token does
// not match what is expected.

func (p *pratt) expect(tok token.TOKEN_TYPE) { 
	if p.peek().Type != tok {
		p.report_expect_error(p.peek(), tok, "expected '%v'")
	}
	p.advance()
	return
}

func (p *pratt) report_parse_error(token token.Token, format_string string) {
	msg := fmt.Sprintf(format_string, token)
	glox_err.Parse_Panic(p.path, token.Line, msg)
}

func (p *pratt) report_offset_parse_error(offset int, format_string string) {
	index := p.current + offset
	tok_at_idx := p.tokens[index]
	msg := fmt.Sprintf(format_string, tok_at_idx)
	glox_err.Parse_Panic(p.path, tok_at_idx.Line, msg)
}

func (p *pratt) report_expect_error(
	token token.Token,
	expected token.TOKEN_TYPE,
	format_string string,
) {
	msg := fmt.Sprintf(format_string, expected)
	glox_err.Parse_Panic(p.path, token.Line, msg)
}

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
	prec_map[token.ASSIGN]  = LOWEST
	prec_map[token.CONST]   = LOWEST
	prec_map[token.WALRUS]  = LOWEST
	prec_map[token.EQL]     = EQUALITY
	prec_map[token.NEQ]     = EQUALITY
	prec_map[token.GEQ]     = LESSGREATER
	prec_map[token.GTR]     = LESSGREATER
	prec_map[token.LEQ]     = LESSGREATER
	prec_map[token.LSS]     = LESSGREATER
	prec_map[token.DECRYBY] = SUB
	prec_map[token.SUB]     = SUB
	prec_map[token.ADD]     = ADD
	prec_map[token.INCRBY]  = ADD
	prec_map[token.MUL]     = MUL
	prec_map[token.QUO]     = QUO
	prec_map[token.DECR]    = UNARY
	prec_map[token.INCR]    = UNARY
	prec_map[token.IDENT]   = PRIMARY

	null_deno[token.BITAND] = nd_parse_pointer_expr
	null_deno[token.BREAK]  = nd_parse_branch_stmt
	null_deno[token.CONST]  = nd_parse_ident_expr
	null_deno[token.DECR]   = nd_parse_unary_expr
	null_deno[token.FALSE]  = nd_parse_literal_expr
	null_deno[token.FLOAT]  = nd_parse_literal_expr
	null_deno[token.IDENT]  = nd_parse_ident_expr
	null_deno[token.INCR]   = nd_parse_unary_expr
	null_deno[token.LBRACE] = nd_parse_block_stmt
	null_deno[token.LPAREN] = nd_parse_paren_expr
	null_deno[token.MUL]    = nd_parse_pointer_expr
	null_deno[token.NOT]    = nd_parse_unary_expr
	null_deno[token.NULL]   = nd_parse_literal_expr
	null_deno[token.RETURN] = nd_parse_return_stmt
	null_deno[token.STRING] = nd_parse_literal_expr
	null_deno[token.SUB]    = nd_parse_unary_expr
	null_deno[token.TRUE]   = nd_parse_literal_expr

	left_deno[token.ADD]     = ld_parse_binary_expr
	left_deno[token.ASSIGN]  = ld_parse_assign_stmt
	left_deno[token.DECRYBY] = ld_parse_binary_expr
	left_deno[token.DECR]    = ld_parse_unary_expr
	left_deno[token.EQL]     = ld_parse_binary_expr
	left_deno[token.GEQ]     = ld_parse_binary_expr
	left_deno[token.GTR]     = ld_parse_binary_expr
	left_deno[token.INCRBY]  = ld_parse_binary_expr
	left_deno[token.INCR]    = ld_parse_unary_expr
	left_deno[token.LEQ]     = ld_parse_binary_expr
	left_deno[token.LSS]     = ld_parse_binary_expr
	left_deno[token.MUL]     = ld_parse_binary_expr
	left_deno[token.NEQ]     = ld_parse_binary_expr
	left_deno[token.QUO]     = ld_parse_binary_expr
	left_deno[token.SUB]     = ld_parse_binary_expr
	left_deno[token.WALRUS]  = ld_parse_decl_stmt
}

func ld_parse_binary_expr(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_binary: operator: %v\n", parser.peek())
	}
	// step past infix operator
	rhs_tok := parser.advance()
	lhs_expr, ok := lhs.(ast.Expr)
	if !ok {
		parser.report_offset_parse_error(-2, "%v: expected expression")
	}
	rhs_expr, ok := parser.parse_node(prec_map[operator]).(ast.Expr)
	if !ok {
		parser.report_parse_error(rhs_tok, "%v: expected expression")
	}
	return &ast.BinaryExpr{
		Lhs:      lhs_expr,
		Operator: operator,
		Rhs:      rhs_expr,
	}
}

func nd_parse_ident_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_indent: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	if tok.Type == token.CONST {
		ident_expr, ok := parser.parse_node(prec_map[tok.Type]).(*ast.IdentExpr)
		if !ok {
			parser.report_offset_parse_error(-1, "%v: expected identifier")
		}
		ident_expr.Mutable = false
		return ident_expr
	}
	return &ast.IdentExpr{
		Name:    tok.Literal,
		Mutable: true,
	}
}

func nd_parse_literal_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_literal: kind: %v, value: %v, next: %v\n",
			tok.Type,
			tok.Literal,
			parser.peek(),
		)
	}
	return &ast.LiteralExpr{Kind: tok.Type, Value: tok.Literal}
}

func nd_parse_paren_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_paren: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	expr, ok := parser.parse_node(prec_map[tok.Type]).(ast.Expr)
	if !ok {
		parser.report_offset_parse_error(-1, "%v: expected expression")
	}
	parser.expect(token.RPAREN)
	return &ast.ParenExpr{Expr: expr}
}

func nd_parse_pointer_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf("nd_pointer: operator %v, next: %v\n", tok, parser.peek())
	}
	ident_expr, ok := parser.parse_node(prec_map[tok.Type]).(*ast.IdentExpr)
	if !ok {
		parser.report_offset_parse_error(-1, "%v: expected identifier")
	}
	deref := false
	if tok.Type == token.MUL {
		deref = true
	}
	return &ast.PtrExpr{Ident: ident_expr, Deref: deref}
}

func nd_parse_unary_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf("nd_unary: operator %v, next: %v\n", tok, parser.peek())
	}
	unary_expr, ok := parser.parse_node(prec_map[tok.Type]).(ast.Expr)
	if !ok {
		parser.report_offset_parse_error(-1, "%v: expected expression")
	}
	return &ast.UnaryExpr{Operator: tok.Type, Rhs: unary_expr}
}

func ld_parse_unary_expr(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_unary: operator: %v\n", parser.peek())
	}
	rhs_tok := parser.advance()
	rhs_expr, ok := lhs.(ast.Expr)
	if !ok {
		parser.report_parse_error(rhs_tok, "%v: expected expression")
	}
	return &ast.UnaryExpr{Operator: operator, Rhs: rhs_expr}
}

func ld_parse_assign_stmt(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Println("ld_assign")
	}
	parser.expect(token.ASSIGN)
	lhs_ident, ok := lhs.(*ast.IdentExpr)
	if !ok {
		parser.report_offset_parse_error(-2, "%v: expected identifier")
	}
	rhs_expr, ok := parser.parse_node(prec_map[operator]).(ast.Expr)
	if !ok {
		parser.report_offset_parse_error(-1, "%v: expected identifier")
	}
	return &ast.AssignStmt{
		Lhs:   lhs_ident,
		Token: operator,
		Rhs:   rhs_expr,
	}
}

func nd_parse_block_stmt(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_block: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	list := parser.parse_stmt_list()
	parser.expect(token.RBRACE)
	return &ast.BlockStmt{List: list}
}

func (p *pratt) parse_stmt_list() []ast.Stmt {
	list := []ast.Stmt{}
	for p.peek().Type != token.RBRACE && p.peek().Type != token.EOF {
		list = append(list, p.parse_stmt())
	}
	return list
}

func (p *pratt) parse_stmt() (stmt ast.Stmt) {
	if p.trace {
		fmt.Printf(
			"stmt: cur_tok: %v\n",
			p.peek(),
		)
	}
	switch p.peek().Type {
	case 
		token.IDENT, token.FLOAT, token.STRING, token.LPAREN, token.ADD, 
		token.SUB, token.MUL, token.AND, token.NOT, token.RETURN:
		expr, ok := p.parse_node(LOWEST - 1).(ast.Stmt)
		if !ok {
			p.report_offset_parse_error(-1, "%v: expected statement")
		}
		stmt = expr
	case token.LBRACE:
		next_tok := p.advance()
		stmt = nd_parse_block_stmt(p, next_tok).(ast.Stmt)
		p.expect(token.SEMICOLON)
	case token.SEMICOLON:
		p.expect(token.SEMICOLON)
	case token.RBRACE:
		stmt = &ast.EmptyStmt{}
	default:
		p.report_parse_error(p.peek(), "%v: expected statement")
	}
	return
}

func nd_parse_branch_stmt(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_branch: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	return &ast.BranchStmt{Token: tok.Type}
}

func ld_parse_decl_stmt(
	parser *pratt,
	operator token.TOKEN_TYPE,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_decl: lhs: %v, operator: %v\n", lhs, operator)
	}
	lhs_ident, ok := lhs.(*ast.IdentExpr)
	if !ok {
		parser.report_offset_parse_error(-2, "%v: expected identifier")
	}
	parser.expect(token.WALRUS)
	rhs := parser.parse_node(prec_map[operator])
	return &ast.DeclStmt{
		Decl: &ast.GenericDecl{
			Name:  lhs_ident,
			Tok:   operator,
			Value: rhs,
		},
	}
}

func nd_parse_return_stmt(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_return: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	result_expr, ok := parser.parse_node(prec_map[tok.Type]).(ast.Expr)
	if !ok {
		parser.report_offset_parse_error(-1, "%v: expected expression")
	}
	return &ast.ReturnStmt{Result: result_expr}
}

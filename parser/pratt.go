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
		trace:   false,
	}
	return pratt
}

func (p *pratt) Parse() []ast.Node {
	var nodes []ast.Node
	for !p.is_at_end() {
		// wrapped in anonymous function so block is called as unit - enables
		// panic recovery to occur mutiple times instead of short-circuiting on
		// first capture; recovery advances to start of next viable Node token, 
		// the operation continues from that point
		func() {
			defer func() {
				if r := recover(); r != nil {
					glox_err.ParsePanicRecover(fmt.Sprint(r))
					recover_and_sync(p)
				}
			}()
			// Safety: panics
			// parse with lower precendence than will encounter during 
			// recursive parse operation to ensure all latter Nodes will be of
			// higher precendence (binding power) and thus accepted
			node := p.parse_node(LOWEST - 1)
			nodes = append(nodes, node)
			if p.trace {
				fmt.Printf("----- PARSED: [%v] -----\n", reflect.TypeOf(node))
			}
		}()
	}
	return nodes
}

func recover_and_sync(parser *pratt) {
	next_tok := parser.advance()
	for !parser.is_at_end() {
		// step past end Node boundary ';' to continue parse operation
		if next_tok.Type == token.SEMICOLON {
			parser.expect(token.SEMICOLON)
			return
		}
		// potential beginning Node boundaries (if formatted correctly)
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
	// NOTE: step past the first token to parse the subexpression - this
	// results in later dispatch methods typically not needing to handle
	// their first 'expected' tokens, parsing should assume it is consumed
	p.advance()
	if p.trace {
		fmt.Printf(
			"parse head: prec %v, cur_tok: %v, next_tok: %v, next_prec: %v\n",
			current_precedence,
			cur_tok,
			p.peek().Type,
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
func (p *pratt) expect(tok token.TokenType) { 
	if p.peek().Type != tok {
		p.report_expect_error(p.peek(), tok, "expected '%v'")
	}
	p.advance()
	return
}

func (p *pratt) report_parse_error(token token.Token, format_string string) {
	msg := fmt.Sprintf(format_string, token)
	glox_err.ParsePanic(p.path, token.Line, msg)
}

func (p *pratt) report_offset_parse_error(offset int, format_string string) {
	index := p.current + offset
	tok_at_idx := p.tokens[index]
	msg := fmt.Sprintf(format_string, tok_at_idx)
	glox_err.ParsePanic(p.path, tok_at_idx.Line, msg)
}

func (p *pratt) report_expect_error(
	token token.Token,
	expected token.TokenType,
	format_string string,
) {
	msg := fmt.Sprintf(format_string, expected)
	glox_err.ParsePanic(p.path, token.Line, msg)
}

type precedence int

const (
	// why go doesn't have proper enums escapes me - the higher the precedence
	// the stronger the relative binding power i.e. the current token will be
	// parsed before its rhs neighbour
	LOWEST precedence = iota
	EQUALITY
	LESSGREATER
	SUB
	ADD
	MUL
	QUO
	UNARY
	PAREN
	PRIMARY
)

var (
	// token type -> operator precedence (binding power)
	prec_map = map[token.TokenType]precedence{}

	// token type -> null denotation; no expression found left of current token
	null_deno = map[token.TokenType]func(
		parser *pratt,
		cur_tok token.Token,
	) ast.Node{}

	// token type -> left denotation; expression found left of current token
	left_deno = map[token.TokenType]func(parser *pratt,
		cur_tok token.TokenType,
		lhs ast.Node,
	) ast.Node{}
)

func init() {
	prec_map[token.ASSIGN]    = LOWEST
	prec_map[token.COLON]     = LOWEST
	prec_map[token.CONST]     = LOWEST
	prec_map[token.FUNASSIGN] = LOWEST
	prec_map[token.RETURN]    = LOWEST
	prec_map[token.WALRUS]    = LOWEST
	prec_map[token.EQL]       = EQUALITY
	prec_map[token.NEQ]       = EQUALITY
	prec_map[token.GEQ]       = LESSGREATER
	prec_map[token.GTR]       = LESSGREATER
	prec_map[token.LEQ]       = LESSGREATER
	prec_map[token.LSS]       = LESSGREATER
	prec_map[token.DECRYBY]   = SUB
	prec_map[token.SUB]       = SUB
	prec_map[token.ADD]       = ADD
	prec_map[token.INCRBY]    = ADD
	prec_map[token.STAR]      = MUL
	prec_map[token.QUO]       = QUO
	prec_map[token.DECR]      = UNARY
	prec_map[token.INCR]      = UNARY
	prec_map[token.LPAREN]    = PAREN
	prec_map[token.IDENT]     = PRIMARY

	null_deno[token.BITAND] = nd_parse_pointer_expr
	null_deno[token.BREAK]  = nd_parse_branch_stmt
	null_deno[token.CONST]  = nd_parse_ident_expr
	null_deno[token.DECR]   = nd_parse_unary_expr
	null_deno[token.FALSE]  = nd_parse_literal_expr
	null_deno[token.S64]    = nd_parse_literal_expr
	null_deno[token.IDENT]  = nd_parse_ident_expr
	null_deno[token.INCR]   = nd_parse_unary_expr
	null_deno[token.LBRACE] = nd_parse_block_stmt
	null_deno[token.LPAREN] = nd_parse_paren_expr
	null_deno[token.STAR]   = nd_parse_pointer_expr
	null_deno[token.NOT]    = nd_parse_unary_expr
	null_deno[token.NULL]   = nd_parse_literal_expr
	null_deno[token.RETURN] = nd_parse_return_stmt
	null_deno[token.STRING] = nd_parse_literal_expr
	null_deno[token.SUB]    = nd_parse_unary_expr
	null_deno[token.TRUE]   = nd_parse_literal_expr

	left_deno[token.ADD]       = ld_parse_binary_expr
	left_deno[token.ASSIGN]    = ld_parse_assign_stmt
	left_deno[token.COLON]     = ld_parse_decl_stmt
	left_deno[token.DECRYBY]   = ld_parse_binary_expr
	left_deno[token.DECR]      = ld_parse_unary_expr
	left_deno[token.EQL]       = ld_parse_binary_expr
	left_deno[token.FUNASSIGN] = ld_parse_decl_stmt
	left_deno[token.GEQ]       = ld_parse_binary_expr
	left_deno[token.GTR]       = ld_parse_binary_expr
	left_deno[token.INCRBY]    = ld_parse_binary_expr
	left_deno[token.INCR]      = ld_parse_unary_expr
	left_deno[token.LEQ]       = ld_parse_binary_expr
	left_deno[token.LPAREN]    = ld_parse_call_expr
	left_deno[token.LSS]       = ld_parse_binary_expr
	left_deno[token.NEQ]       = ld_parse_binary_expr
	left_deno[token.PERIOD]    = ld_parse_selector_expr
	left_deno[token.QUO]       = ld_parse_binary_expr
	left_deno[token.STAR]      = ld_parse_binary_expr
	left_deno[token.SUB]       = ld_parse_binary_expr
	left_deno[token.WALRUS]    = ld_parse_decl_stmt
}

func ld_parse_binary_expr(
	parser *pratt,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_binary: operator: %v\n", operator)
	}
	lhs_expr := parser.as_expr(lhs)
	parser.advance() // step past infix operator
	rhs_expr := parser.parse_basic_expr(operator)
	return &ast.BinOp{
		Lhs:      lhs_expr,
		Operator: operator,
		Rhs:      rhs_expr,
	}
}

func ld_parse_call_expr(
	parser *pratt,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_call: operator: %v\n", operator)
	}
	lhs_ident := parser.as_ident(lhs)
	lhs_ident.Obj.Kind = ast.Procedure
	args := parser.parse_call_args() 
	parser.expect(token.RPAREN)
	return &ast.CallExpr{Ident: lhs_ident, Args: args}
}

func nd_parse_ident_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_ident: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek().Type,
		)
	}
	if tok.Type == token.CONST {
		ident_expr := parser.parse_basic_ident(tok.Type)
		ident_expr.Obj.Kind = ast.Constant
		ident_expr.Mutable = false
		return ident_expr
	}
	return &ast.Ident{
		Obj: &ast.Object{
			Kind: ast.Variable,
			Name: tok.Literal,
		},
		Mutable: true,
	}
}

func nd_parse_literal_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_literal: cur_tok: %v, next: %v\n",
			tok,
			parser.peek().Type,
		)
	}
	typ := parser.get_type(tok)
	return &ast.LiteralExpr{Type: typ, Value: tok.Literal}
}

func nd_parse_paren_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_paren: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	expr := parser.parse_basic_expr(parser.peek().Type)
	parser.expect(token.RPAREN)
	return &ast.ParenExpr{Expr: expr}
}

func nd_parse_pointer_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_pointer: operator %v, next: %v\n",
			tok.Type,
			parser.peek(),
		)
	}
	ident_expr := parser.parse_basic_ident(tok.Type)
	deref := false
	if tok.Type == token.STAR {
		deref = true
	}
	return &ast.PtrExpr{Ident: ident_expr, Deref: deref}
}

func nd_parse_unary_expr(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_unary: operator %v, next: %v\n",
			tok.Type,
			parser.peek(),
		)
	}
	unary_expr := parser.parse_basic_expr(tok.Type)
	return &ast.UnOp{Operator: tok.Type, Rhs: unary_expr}
}

func ld_parse_assign_stmt(
	parser *pratt,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_assign: operator: %v\n", operator)
	}
	parser.expect(token.ASSIGN)
	identifer := parser.as_ident(lhs)
	rhs := parser.parse_basic_expr(operator)
	identifer.Obj.Data = rhs
	return &ast.AssignStmt{Ident: identifer}
}

func ld_parse_selector_expr(
	parser *pratt,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_selector: operator: %v\n", operator)
	}
	lhs_expr := parser.as_ident(lhs)
	parser.expect(token.PERIOD)
	selection := parser.parse_basic_expr(operator)
	return &ast.SelectorExpr{Parent: lhs_expr, Selection: selection}
}

func ld_parse_unary_expr(
	parser *pratt,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_unary: operator: %v\n", operator)
	}
	rhs_expr := parser.as_expr(lhs)
	parser.advance() // step past postfix operator
	return &ast.UnOp{Operator: operator, Rhs: rhs_expr}
}

func nd_parse_block_stmt(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_block: cur_tok: %v, next_tok: %v\n",
			tok.Literal,
			parser.peek(),
		)
	}
	if parser.peek().Type == token.RBRACE {
		parser.expect(token.RBRACE)
		return &ast.EmptyStmt{}
	}
	list := parser.parse_stmt_list()
	parser.expect(token.RBRACE)
	return &ast.BlockStmt{List: list}
}

func (p *pratt) parse_stmt_list() []ast.Stmt {
	list := []ast.Stmt{}
	var stmt ast.Stmt
	for p.peek().Type != token.RBRACE && p.peek().Type != token.EOF {
		if p.trace {
			fmt.Printf(
				"stmt_list: cur_tok: %v\n",
				p.peek().Type,
			)
		}
		switch p.peek().Type {
		case 
			token.IDENT, token.S64, token.STRING, token.LPAREN, token.ADD,
			token.SUB, token.STAR, token.AND, token.NOT, token.RETURN, 
			token.BREAK, token.CONST:
			node := p.parse_node(LOWEST - 1) 
			stmt, ok := node.(ast.Stmt)
			if !ok {
				stmt = p.try_make_statement(node)
				if stmt == nil {
					p.report_offset_parse_error(-1, "%v: expected statement")
					continue
				}
			}
			list = append(list, stmt)
		case token.LBRACE:
			p.expect(token.LBRACE)
			stmt = nd_parse_block_stmt(p, p.peek()).(ast.Stmt)
			list = append(list, stmt)
		case token.SEMICOLON:
			p.expect(token.SEMICOLON)
		default:
			p.report_parse_error(p.peek(), "%v: expected statement")
		}
	}
	return list
}

func nd_parse_branch_stmt(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf("nd_branch: cur_tok: %v\n", tok)
	}
	return &ast.BranchStmt{}
}

func ld_parse_decl_stmt(
	parser *pratt,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_decl: operator: %v\n", operator)
	}
	lhs_ident := parser.as_ident(lhs)
	parser.advance() // step past declaration operator
	var decl ast.Decl
	switch operator {
	case token.COLON:
		decl = parser.parse_typed_declaration(lhs_ident, operator)
	case token.FUNASSIGN:
		if parser.peek().Type == token.STRUCT {
			decl = parser.parse_struct_declaration(lhs_ident, operator)
		} else {
			decl = parser.parse_procedure_declaration(lhs_ident, operator)
		}
	case token.WALRUS:
		decl = parser.parse_generic_declaration(lhs_ident, operator)
	}
	return &ast.DeclStmt{Decl: decl}
}

func (p *pratt) parse_procedure_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.ProcedureDecl {
	if p.trace {
		fmt.Println("Procedure_decl")
	}
	lhs_ident.Obj.Kind = ast.Procedure
	args := p.parse_procedure_args(lhs_ident.Obj.Name)
	lhs_ident.Obj.Decl = args
	p.expect(token.RPAREN)
	p.expect(token.FUNRETURN)
	var typ ast.Typ
	if p.peek().Type == token.LBRACE {
		typ = ast.NullType // no return type
	} else {
		typ = p.get_type(p.peek())
		p.advance() // step past type
	}
	lhs_ident.Obj.Type = typ
	p.expect(token.LBRACE) // step into block statement 
	body := p.as_block(nd_parse_block_stmt(p, p.peek()))
	return &ast.ProcedureDecl{Ident: lhs_ident, Body: body}
}

func (p *pratt) parse_generic_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	if p.trace {
		fmt.Println("basic_decl")
	}
	lhs_ident.Obj.Kind = ast.Variable
	rhs_expr := p.parse_basic_expr(operator)
	return &ast.BasicDecl{Ident: lhs_ident, Value: rhs_expr}
}

func (p *pratt) parse_struct_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	if p.trace {
		fmt.Println("typed_decl")
	}
	p.expect(token.STRUCT)
	p.expect(token.LBRACE) // step into struct fields
	field_list := p.parse_field_list()
	p.expect(token.RBRACE)
	struct_type := &ast.StructType{Fields: field_list}
	return &ast.BasicDecl{Ident: lhs_ident, Value: struct_type}
}

func (p *pratt) parse_typed_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	if p.trace {
		fmt.Println("typed_decl")
	}
	typ := p.get_type(p.peek())
	lhs_ident.Obj.Type = typ
	lhs_ident.Obj.Kind = ast.Variable
	p.advance() // step past type
	p.expect(token.ASSIGN)
	rhs_expr := p.parse_basic_expr(operator)
	return &ast.BasicDecl{Ident: lhs_ident, Value: rhs_expr}
}

func nd_parse_return_stmt(parser *pratt, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_return: next_tok: %v\n",
			parser.peek(),
		)
	}
	result_expr := parser.parse_basic_expr(tok.Type)
	return &ast.ReturnStmt{Result: result_expr}
}

func(p *pratt) parse_call_args() []ast.Expr {
	p.expect(token.LPAREN)
	args := []ast.Expr{}
	var expr ast.Expr
	for p.peek().Type != token.RPAREN {
		if p.peek().Type == token.COMMA {
			p.expect(token.COMMA)
			continue
		}
		if p.peek().Type == token.IDENT {
			expr = p.parse_basic_expr(p.peek().Type)
			if p.peek().Type == token.LPAREN {
				expr = ld_parse_call_expr(p, p.peek().Type, expr).(ast.Expr)
			}
		} else {
			expr = p.parse_basic_expr(p.peek().Type)
		}
		args = append(args, expr)
	}
	return args
}

func (p *pratt) parse_field_list() []*ast.Field {
	fields := []*ast.Field{}
	for p.peek().Type != token.RBRACE {
		if p.peek().Type == token.SEMICOLON {
			p.expect(token.SEMICOLON)
			continue
		}
		name := p.parse_basic_ident(p.peek().Type)
		p.expect(token.COLON)
		typ := p.get_type(p.peek())
		name.Obj.Kind = ast.Type
		name.Obj.Type = typ
		p.advance() // step past type
		field := &ast.Field{Names: []*ast.Ident{name}}
		fields = append(fields, field)
	}
	return fields
}

func(p *pratt) parse_procedure_args(identifier string) ast.Environment {
	p.expect(token.LPAREN)
	scope := ast.Environment{}
	for p.peek().Type != token.RPAREN {
		if p.peek().Type == token.COMMA {
			p.expect(token.COMMA)
			continue
		}
		ident_object := p.parse_basic_ident(p.peek().Type).Obj
		p.expect(token.COLON)
		typ := p.get_type(p.peek())
		ident_object.Type = typ
		p.advance() // step past type
		scope[identifier] = append(scope[identifier], ident_object)
	}
	return scope
}

func (p *pratt) get_type(tok token.Token) ast.Typ {
	var typ ast.Typ
	switch tok.Type {
	case token.FALSE, token.TRUE, token.BOOLTYPE:
		typ = ast.BoolType
	case token.S64, token.S64TYPE:
		typ = ast.S64Type
	case token.NULL:
		typ = ast.NullType
	case token.STRING, token.STRINGTYPE:
		typ = ast.StringType
	default:
		// TODO: simple solution for now, could embed the type here
		typ = ast.Custom
	}
	return typ
}

// parses the next token as an expression using the supplied tok precedence
func(p *pratt) parse_basic_expr(tok token.TokenType) ast.Expr {
	expr, ok := p.parse_node(prec_map[tok]).(ast.Expr)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected expression")
	}
	return expr
}

// parses the next token as an identifier using the supplied tok precedence
func(p *pratt) parse_basic_ident(tok token.TokenType) *ast.Ident {
	ident, ok := p.parse_node(prec_map[tok]).(*ast.Ident)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected identifier")
	}
	return ident
}

// casts node to a block statement; raises an error if the cast fails
func(p *pratt) as_block(node ast.Node) *ast.BlockStmt {
	block, ok := node.(*ast.BlockStmt)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected block statement")
	}
	return block
}

// casts node to an expression; raises an error if the cast fails
func(p *pratt) as_expr(node ast.Node) ast.Expr {
	expr, ok := node.(ast.Expr)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected expression")
	}
	return expr
}

// casts node to an identifier; raises an error if the cast fails
func(p *pratt) as_ident(node ast.Node) *ast.Ident {
	ident, ok := node.(*ast.Ident)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected identifier")
	}
	return ident
}

func (p *pratt) try_make_statement(node ast.Node) ast.Stmt {
	var stmt ast.Stmt
	maybe_expr, ok := node.(ast.Expr)
	if !ok {
		p.report_parse_error(p.peek(), "%v: expected expression")
		return nil
	}
	// for simplicity only expressions that have side effects can appear
	// standalone within a block
	switch maybe_expr.(type) {
	case *ast.CallExpr:
		stmt = &ast.ExprStmt{Expr: maybe_expr}
	case *ast.BinOp:
		binary_expr := maybe_expr.(*ast.BinOp)
		if binary_expr.Operator == token.INCRBY || 
			binary_expr.Operator == token.DECRYBY {
			stmt = &ast.ExprStmt{Expr: binary_expr}
			break
		}
		p.report_offset_parse_error(-2, "%v: cannot cast to statement")
	case *ast.UnOp:
		unary_expr := maybe_expr.(*ast.UnOp)
		if unary_expr.Operator == token.INCR || 
			unary_expr.Operator == token.DECR {
			stmt = &ast.ExprStmt{Expr: unary_expr}
			break
		}
		p.report_offset_parse_error(-2, "%v: cannot cast to statement")
	}
	return stmt
}

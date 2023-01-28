package parser

import (
	"glox/ast"
	"glox/token"
)

type precedence int

const (
	// why go doesn't have proper enums escapes me - the higher the precedence
	// the stronger the relative binding power i.e. the current token will be
	// parsed before its rhs neighbour
	INIT precedence = -1

	LOWEST precedence = iota
	LOGICAL
	EQUALITY
	LESSGREATER
	SUB
	ADD
	MUL
	QUO
	UNARY
	METHOD
	PAREN
	CAST
	PRIMARY
)

var (
	// token type -> operator precedence (binding power)
	prec_map = map[token.TokenType]precedence{}

	// token type -> null denotation parser i.e. dispatches a token specific
	// parser when no expression was found left of the current token
	null_deno = map[token.TokenType]func(*parser, token.Token) ast.Node{}

	// token type -> left denotation parser i.e. dispatches a token specific
	// parser when an expression was found left of the current token
	left_deno = map[token.TokenType]func(*parser, token.TokenType, ast.Node) ast.Node{}
)

func init() {
	prec_map[token.ASSIGN]    = LOWEST
	prec_map[token.COLON]     = LOWEST
	prec_map[token.CONST]     = LOWEST
	prec_map[token.FUNASSIGN] = LOWEST
	prec_map[token.IF]        = LOWEST
	prec_map[token.RETURN]    = LOWEST
	prec_map[token.WALRUS]    = LOWEST
	prec_map[token.WHILE]     = LOWEST
	prec_map[token.AND]       = LOGICAL
	prec_map[token.OR]        = LOGICAL
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
	prec_map[token.PERIOD]    = METHOD
	prec_map[token.LPAREN]    = PAREN
	prec_map[token.CAST]      = CAST
	prec_map[token.IDENT]     = PRIMARY

	null_deno[token.BITAND]   = nd_parse_pointer_expr
	null_deno[token.BOOLTYPE] = nd_parse_literal_expr
	null_deno[token.BREAK]    = nd_parse_branch_stmt
	null_deno[token.CONST]    = nd_parse_ident_expr
	null_deno[token.DECR]     = nd_parse_unary_expr
	null_deno[token.F64TYPE]  = nd_parse_literal_expr
	null_deno[token.F64]      = nd_parse_literal_expr
	null_deno[token.FALSE]    = nd_parse_literal_expr
	null_deno[token.IDENT]    = nd_parse_ident_expr
	null_deno[token.IF]       = nd_parse_if_stmt
	null_deno[token.INCR]     = nd_parse_unary_expr
	null_deno[token.LBRACE]   = nd_parse_block_stmt
	null_deno[token.LPAREN]   = nd_parse_paren_expr
	null_deno[token.NOT]      = nd_parse_unary_expr
	null_deno[token.NULL]     = nd_parse_literal_expr
	null_deno[token.RETURN]   = nd_parse_return_stmt
	null_deno[token.S64TYPE]  = nd_parse_literal_expr
	null_deno[token.S64]      = nd_parse_literal_expr
	null_deno[token.STAR]     = nd_parse_pointer_expr
	null_deno[token.STRING]   = nd_parse_literal_expr
	null_deno[token.SUB]      = nd_parse_unary_expr
	null_deno[token.TRUE]     = nd_parse_literal_expr
	null_deno[token.WHILE]    = nd_parse_while_stmt

	left_deno[token.ADD]       = ld_parse_binary_expr
	left_deno[token.AND]       = ld_parse_binary_expr
	left_deno[token.ASSIGN]    = ld_parse_assign_stmt
	left_deno[token.CAST]      = ld_parse_cast_expr
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
	left_deno[token.OR]        = ld_parse_binary_expr
	left_deno[token.PERIOD]    = ld_parse_selector_expr
	left_deno[token.QUO]       = ld_parse_binary_expr
	left_deno[token.STAR]      = ld_parse_binary_expr
	left_deno[token.SUB]       = ld_parse_binary_expr
	left_deno[token.WALRUS]    = ld_parse_decl_stmt
}

/* NULL DENOTATION PARSERS */

func nd_parse_block_stmt(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_block", tok)
	list := p.parse_stmt_list()
	return &ast.BlockStmt{List: list}
}

func nd_parse_branch_stmt(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_branch", tok)
	return &ast.BranchStmt{Token: tok.Type}
}

func nd_parse_if_stmt(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_if", tok)
	predicate := p.parse_until(token.LBRACE)
	body := p.as_block(p.parse_basic_stmt(p.peek().Type))
	p.expect(token.RBRACE)
	var else_body ast.Stmt
	if p.peek().Type == token.ELSE {
		p.expect(token.ELSE)
		else_body = p.parse_basic_stmt(p.peek().Type)
		return &ast.IfStmt{Predicate: predicate, Body: body, Else: else_body}
	}
	return &ast.IfStmt{Predicate: predicate, Body: body, Else: else_body}
}

func nd_parse_ident_expr(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_ident", tok)
	if tok.Type == token.CONST {
		ident_expr := p.parse_basic_ident(tok.Type)
		ident_expr.Obj.Kind = ast.Constant
		ident_expr.Obj.Mutable = false
		return ident_expr
	}
	typ := p.get_type(tok)
	return &ast.Ident{
		Obj: &ast.Object{
			Kind: ast.Variable,
			Name: tok.Lit,
			Type: typ,
			Mutable: true,
		},
	}
}

func nd_parse_literal_expr(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_literal", tok)
	typ := p.get_type(tok)
	return &ast.LiteralExpr{Type: typ, Value: tok.Lit}
}

func nd_parse_paren_expr(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_paren", tok)
	expr := p.parse_basic_expr(p.peek().Type)
	p.expect(token.RPAREN)
	return &ast.ParenExpr{Expr: expr}
}

func nd_parse_pointer_expr(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_pointer", tok)
	ident_expr := p.parse_basic_ident(tok.Type)
	deref := false
	if tok.Type == token.STAR { deref = true }
	return &ast.PtrExpr{Ident: ident_expr, Deref: deref}
}

func nd_parse_return_stmt(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_return", tok)
	result_expr := p.parse_basic_expr(tok.Type)
	return &ast.ReturnStmt{Result: result_expr}
}

func nd_parse_unary_expr(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_unary", tok)
	unary_expr := p.parse_basic_expr(tok.Type)
	return &ast.UnOp{Operator: tok.Type, Rhs: unary_expr}
}

func nd_parse_while_stmt(p *parser, tok token.Token) ast.Node {
	p.trace_nd("nd_while", tok)
	predicate := p.parse_until(token.LBRACE)
	body := p.as_block(p.parse_basic_stmt(p.peek().Type))
	return &ast.WhileStmt{Predicate: predicate, Body: body}
}

/* LEFT DENOTATION PARSERS */

func ld_parse_assign_stmt(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_assign", tok)
	p.expect(token.ASSIGN)
	identifer := p.as_expr(lhs)
	rhs := p.parse_basic_expr(tok)
	return &ast.AssignStmt{Lhs: identifer, Rhs: rhs}
}

func ld_parse_binary_expr(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_binary", tok)
	lhs_expr := p.as_expr(lhs)
	p.advance() // step past infix operator
	rhs_expr := p.parse_basic_expr(tok)
	return &ast.BinOp{Lhs: lhs_expr, Operator: tok, Rhs: rhs_expr}
}

func ld_parse_call_expr(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_call", tok)
	lhs_ident := p.as_ident(lhs)
	lhs_ident.Obj.Kind = ast.Call
	args := p.parse_call_args()
	p.expect(token.RPAREN)
	return &ast.CallExpr{Ident: lhs_ident, Args: args}
}

func ld_parse_cast_expr(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_cast", tok)
	literal := p.as_literal(lhs)
	p.expect(token.CAST)
	rhs := p.parse_basic_expr(tok)
	return &ast.CastExpr{To: literal.Type, From: rhs}
}

func ld_parse_decl_stmt(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_decl", tok)
	lhs_ident := p.as_ident(lhs)
	p.advance() // step past declaration operator
	var decl ast.Decl
	switch tok {
	case token.COLON:
		decl = p.parse_typed_declaration(lhs_ident, tok)
	case token.FUNASSIGN:
		switch p.peek().Type {
		case token.STRUCT:
			decl = p.parse_struct_declaration(lhs_ident, tok)
		case token.LPAREN:
			decl = p.parse_procedure_declaration(lhs_ident, tok)
		case token.INTERFACE:
			decl = p.parse_interface_declaration(lhs_ident, tok)
		}
	case token.WALRUS:
		decl = p.parse_basic_decl(lhs_ident, tok)
	}
	return &ast.DeclStmt{Decl: decl}
}

func ld_parse_selector_expr(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_selector", tok)
	lhs_expr := p.as_expr(lhs)
	p.expect(token.PERIOD)
	selection := p.parse_basic_expr(tok)
	return &ast.SelectorExpr{Parent: lhs_expr, Selection: selection}
}

func ld_parse_unary_expr(p *parser, tok token.TokenType, lhs ast.Node) ast.Node {
	p.trace_ld("ld_unary", tok)
	rhs_expr := p.as_expr(lhs)
	p.advance() // step past postfix operator
	return &ast.UnOp{Operator: tok, Rhs: rhs_expr}
}

/* PARSER SPECIFIC UTILS */

func (p *parser) parse_basic_decl(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	rhs_expr := p.parse_basic_expr(operator)
	return &ast.BasicDecl{Ident: lhs_ident, Value: rhs_expr}
}

func (p *parser) parse_call_args() []ast.Expr {
	p.expect(token.LPAREN)
	args := []ast.Expr{}
	var expr ast.Expr
	for p.peek().Type != token.RPAREN {
		if p.peek().Type == token.COMMA {
			p.expect(token.COMMA)
			continue
		}
		expr = p.parse_basic_expr(p.peek().Type)
		args = append(args, expr)
	}
	return args
}

func (p *parser) parse_fields_between(left, right, delim token.TokenType) *ast.Fields {
	p.expect(left)
	fields := ast.Fields{}
	for p.peek().Type != right {
		if p.peek().Type == token.SEMICOLON || p.peek().Type == token.COMMA {
			p.advance()
			continue
		}
		name := p.parse_basic_ident(p.peek().Type)
		p.expect(delim)
		typ := p.get_type(p.peek())
		name.Obj.Kind = ast.Type
		name.Obj.Type = typ
		p.advance() // step past type
		fields.Names = append(fields.Names, name)
	}
	return &fields
}

func (p *parser) parse_interface_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	lhs_ident.Obj.Kind = ast.Interface
	p.expect(token.INTERFACE)
	p.expect(token.LBRACE)
	fields := ast.Fields{}
	for p.peek().Type != token.RBRACE {
		if p.peek().Type == token.SEMICOLON {
			p.expect(token.SEMICOLON)
			continue
		}
		method_ident := p.parse_interface_method()
		fields.Names = append(fields.Names, method_ident)
	}
	p.expect(token.RBRACE)
	iface_type := &ast.IfaceType{Methods: &fields}
	return &ast.BasicDecl{Ident: lhs_ident, Value: iface_type}
}

func (p *parser) parse_interface_method() *ast.Ident {
	ident := p.as_ident(p.parse_until(token.LPAREN))
	fields := p.parse_fields_between(token.LPAREN, token.RPAREN, token.COLON)
	p.expect(token.RPAREN)
	var return_type ast.Typ
	if p.peek().Type == token.FUNRETURN {
		p.expect(token.FUNRETURN)
		return_type = p.get_type(p.peek())
		p.advance() // step past type
	} else {
		return_type = ast.NullType
	}
	ident.Obj.Decl = fields
	ident.Obj.Type = return_type
	return ident
}

func (p *parser) parse_procedure_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.ProcedureDecl {
	lhs_ident.Obj.Kind = ast.Procedure
	fields := p.parse_fields_between(token.LPAREN, token.RPAREN, token.COLON)
	env := ast.Environment{}
	for _, arg := range fields.Names {  env[arg.Obj.Name] = arg.Obj }
	lhs_ident.Obj.Decl = env
	p.expect(token.RPAREN)
	var typ ast.Typ
	if p.peek().Type == token.FUNRETURN {
		p.expect(token.FUNRETURN)
		typ = p.get_type(p.peek())
		p.advance() // step past type
	} else {
		typ = ast.NullType
	}
	lhs_ident.Obj.Type = typ
	body := p.as_block(p.parse_basic_stmt(p.peek().Type))
	p.expect(token.RBRACE)
	return &ast.ProcedureDecl{Ident: lhs_ident, Body: body}
}

func (p *parser) parse_stmt_list() []ast.Stmt {
	stmt_list := []ast.Stmt{}
	var stmt ast.Stmt
	for p.peek().Type != token.RBRACE && p.peek().Type != token.EOF {
		p.trace_ld("stmt_list:", p.peek().Type)
		node := p.parse_node(INIT)
		if maybe_stmt, ok := node.(ast.Stmt); !ok {
			stmt = p.try_make_statement(node)
			if stmt == nil { p.report_offset_parse_error(-1, "%v: expected statement") }
			p.trace_node("PARSED SUB NODE", stmt)
			stmt_list = append(stmt_list, stmt)
		} else {
			p.trace_node("PARSED SUB NODE", maybe_stmt)
			stmt_list = append(stmt_list, maybe_stmt)
		}
	}
	return stmt_list
}

func (p *parser) parse_struct_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	p.expect(token.STRUCT)
	fields := p.parse_fields_between(token.LBRACE, token.RBRACE, token.COLON)
	p.expect(token.RBRACE)
	struct_type := &ast.StructType{Fields: fields}
	return &ast.BasicDecl{Ident: lhs_ident, Value: struct_type}
}

func (p *parser) parse_typed_declaration(
	lhs_ident *ast.Ident,
	operator token.TokenType,
) *ast.BasicDecl {
	typ := p.get_type(p.peek())
	lhs_ident.Obj.Type = typ
	lhs_ident.Obj.Kind = ast.Variable
	p.advance() // step past type
	p.expect(token.ASSIGN)
	rhs_expr := p.parse_basic_expr(operator)
	return &ast.BasicDecl{Ident: lhs_ident, Value: rhs_expr}
}

func (p *parser) try_make_statement(node ast.Node) ast.Stmt {
	var stmt ast.Stmt
	maybe_expr, ok := node.(ast.Expr)
	if !ok { p.report_parse_error(p.peek(), "%v: expected statement") }
	// for simplicity only expressions that have side effects can appear
	// standalone within a block
	switch maybe_stmt := maybe_expr.(type) {
	case *ast.CallExpr:
		stmt = &ast.ExprStmt{Expr: maybe_stmt}
	case *ast.SelectorExpr:
		if _, ok = maybe_stmt.Selection.(*ast.CallExpr); !ok {
			p.report_offset_parse_error(-2, "%v: expected statement")
		}
		stmt = &ast.ExprStmt{Expr: maybe_stmt}
	case *ast.BinOp:
		if maybe_stmt.Operator == token.INCRBY || maybe_stmt.Operator == token.DECRYBY {
			stmt = &ast.ExprStmt{Expr: maybe_stmt}
			break
		}
		p.report_offset_parse_error(-2, "%v: expected statement")
	case *ast.UnOp:
		if maybe_stmt.Operator == token.INCR || maybe_stmt.Operator == token.DECR {
			stmt = &ast.ExprStmt{Expr: maybe_stmt}
			break
		}
		p.report_offset_parse_error(-2, "%v: expected statement")
	}
	return stmt
}

func (p *parser) parse_until(tok token.TokenType) ast.Expr {
	tokens := []token.Token{}
	for p.peek().Type != tok {
		tokens = append(tokens, p.peek())
		p.advance()
	}
	tokens = append(tokens, token.Token{Type: token.EOF, Lit: ""})
	sub_parser := New(p.path, &tokens)
	predicate := sub_parser.parse_node(LOWEST)
	return p.as_expr(predicate)
}

/* PARSER GENERIC UTILS */

func (p *parser) get_type(tok token.Token) ast.Typ {
	var typ ast.Typ
	switch tok.Type {
	case token.FALSE, token.TRUE, token.BOOLTYPE:
		typ = ast.BoolType
	case token.F64, token.F64TYPE:
		typ = ast.F64Type
	case token.S64, token.S64TYPE:
		typ = ast.S64Type
	case token.NULL:
		typ = ast.NullType
	case token.STRING, token.STRINGTYPE:
		typ = ast.StringType
	case token.EOF, token.LBRACE, token.SEMICOLON:
		p.report_parse_error(tok, "%v expected type")
	default:
		// TODO: simple solution for now, could embed the type here or
		// recompile the ast including this new type signature
		typ = ast.Custom
	}
	return typ
}

// parses the next token as an expression using the supplied tok precedence
func (p *parser) parse_basic_expr(tok token.TokenType) ast.Expr {
	expr, ok := p.parse_node(prec_map[tok]).(ast.Expr)
	if !ok { p.report_offset_parse_error(-1, "%v: expected expression") }
	return expr
}

// parses the next token as an identifier using the supplied tok precedence
func (p *parser) parse_basic_ident(tok token.TokenType) *ast.Ident {
	ident, ok := p.parse_node(prec_map[tok]).(*ast.Ident)
	if !ok { p.report_offset_parse_error(-1, "%v: expected identifier") }
	return ident
}

// parses the next token as a statement using the supplied tok precedence
func (p *parser) parse_basic_stmt(tok token.TokenType) ast.Stmt {
	stmt, ok := p.parse_node(prec_map[tok]).(ast.Stmt)
	if !ok { p.report_offset_parse_error(-1, "%v: expected statement") }
	return stmt
}

// casts node to a block statement; raises an error if the cast fails
func (p *parser) as_block(node ast.Node) *ast.BlockStmt {
	block, ok := node.(*ast.BlockStmt)
	if !ok { p.report_offset_parse_error(-1, "%v: expected block statement") }
	return block
}

// casts node to an expression; raises an error if the cast fails
func (p *parser) as_expr(node ast.Node) ast.Expr {
	expr, ok := node.(ast.Expr)
	if !ok { p.report_offset_parse_error(-1, "%v: expected expression") }
	return expr
}

// casts node to an identifier; raises an error if the cast fails
func (p *parser) as_ident(node ast.Node) *ast.Ident {
	ident, ok := node.(*ast.Ident)
	if !ok { p.report_offset_parse_error(-1, "%v: expected identifier") }
	return ident
}

// casts node to a literal; raises an error if the cast fails
func (p *parser) as_literal(node ast.Node) *ast.LiteralExpr {
	ident, ok := node.(*ast.LiteralExpr)
	if !ok { p.report_offset_parse_error(-1, "%v: expected literal") }
	return ident
}
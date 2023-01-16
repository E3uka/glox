package parser

import (
	"fmt"
	"glox/ast"
	"glox/token"
)

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

	left_deno[token.ADD]       = ld_parse_binary_expr
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
	left_deno[token.PERIOD]    = ld_parse_selector_expr
	left_deno[token.QUO]       = ld_parse_binary_expr
	left_deno[token.STAR]      = ld_parse_binary_expr
	left_deno[token.SUB]       = ld_parse_binary_expr
	left_deno[token.WALRUS]    = ld_parse_decl_stmt
}

/* NULL DENOTATION PARSERS */

func nd_parse_block_stmt(parser *parser, tok token.Token) ast.Node {
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

func nd_parse_branch_stmt(parser *parser, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf("nd_branch: cur_tok: %v\n", tok)
	}
	return &ast.BranchStmt{Token: tok.Type}
}

func nd_parse_ident_expr(parser *parser, tok token.Token) ast.Node {
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

func nd_parse_literal_expr(parser *parser, tok token.Token) ast.Node {
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

func nd_parse_paren_expr(parser *parser, tok token.Token) ast.Node {
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

func nd_parse_pointer_expr(parser *parser, tok token.Token) ast.Node {
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

func nd_parse_return_stmt(parser *parser, tok token.Token) ast.Node {
	if parser.trace {
		fmt.Printf(
			"nd_return: next_tok: %v\n",
			parser.peek(),
		)
	}
	result_expr := parser.parse_basic_expr(tok.Type)
	return &ast.ReturnStmt{Result: result_expr}
}

func nd_parse_unary_expr(parser *parser, tok token.Token) ast.Node {
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

/* LEFT DENOTATION PARSERS */

func ld_parse_assign_stmt(
	parser *parser,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_assign: operator: %v\n", operator)
	}
	parser.expect(token.ASSIGN)
	identifer := parser.as_expr(lhs)
	rhs := parser.parse_basic_expr(operator)
	return &ast.AssignStmt{Lhs: identifer, Rhs: rhs}
}

func ld_parse_binary_expr(
	parser *parser,
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
	parser *parser,
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

func ld_parse_cast_expr(
	parser *parser,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_cast: operator: %v\n", operator)
	}
	literal := parser.as_literal(lhs)
	parser.expect(token.CAST)
	rhs := parser.parse_basic_expr(operator)
	return &ast.CastExpr{To: literal.Type, From: rhs}
}

func ld_parse_decl_stmt(
	parser *parser,
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

func ld_parse_selector_expr(
	parser *parser,
	operator token.TokenType,
	lhs ast.Node,
) ast.Node {
	if parser.trace {
		fmt.Printf("ld_selector: operator: %v\n", operator)
	}
	lhs_expr := parser.as_expr(lhs)
	parser.expect(token.PERIOD)
	selection := parser.parse_basic_expr(operator)
	return &ast.SelectorExpr{Parent: lhs_expr, Selection: selection}
}

func ld_parse_unary_expr(
	parser *parser,
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

/* PARSER SPECIFIC UTILS */

func(p *parser) parse_call_args() []ast.Expr {
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

func (p *parser) parse_field_list() []*ast.Field {
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

func (p *parser) parse_generic_declaration(
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

func(p *parser) parse_procedure_args(identifier string) ast.Environment {
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

func (p *parser) parse_procedure_declaration(
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

func (p *parser) parse_stmt_list() []ast.Stmt {
	list := []ast.Stmt{}
	var stmt ast.Stmt
	for p.peek().Type != token.RBRACE && p.peek().Type != token.EOF {
		if p.trace {
			fmt.Printf(
				"stmt_list: cur_tok: %v\n",
				p.peek().Literal,
			)
		}
		switch p.peek().Type {
		case 
			token.IDENT, token.F64, token.S64, token.STRING, token.LPAREN,
			token.ADD, token.SUB, token.STAR, token.AND, token.NOT,
			token.RETURN, token.BREAK, token.CONST:
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

func (p *parser) parse_struct_declaration(
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

func (p *parser) parse_typed_declaration(
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

func (p *parser) try_make_statement(node ast.Node) ast.Stmt {
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
	case *ast.SelectorExpr:
		// TODO: hack; could just be a field selection instead of a call which
		// would be semantically wrong
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
	default:
		// TODO: simple solution for now, could embed the type here or
		// recompile the ast including this new type signature
		typ = ast.Custom
	}
	return typ
}

// parses the next token as an expression using the supplied tok precedence
func(p *parser) parse_basic_expr(tok token.TokenType) ast.Expr {
	expr, ok := p.parse_node(prec_map[tok]).(ast.Expr)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected expression")
	}
	return expr
}

// parses the next token as an identifier using the supplied tok precedence
func(p *parser) parse_basic_ident(tok token.TokenType) *ast.Ident {
	ident, ok := p.parse_node(prec_map[tok]).(*ast.Ident)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected identifier")
	}
	return ident
}

// casts node to a block statement; raises an error if the cast fails
func(p *parser) as_block(node ast.Node) *ast.BlockStmt {
	block, ok := node.(*ast.BlockStmt)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected block statement")
	}
	return block
}

// casts node to an expression; raises an error if the cast fails
func(p *parser) as_expr(node ast.Node) ast.Expr {
	expr, ok := node.(ast.Expr)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected expression")
	}
	return expr
}

// casts node to an identifier; raises an error if the cast fails
func(p *parser) as_ident(node ast.Node) *ast.Ident {
	ident, ok := node.(*ast.Ident)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected identifier")
	}
	return ident
}

// casts node to an identifier; raises an error if the cast fails
func(p *parser) as_literal(node ast.Node) *ast.LiteralExpr {
	ident, ok := node.(*ast.LiteralExpr)
	if !ok {
		p.report_offset_parse_error(-1, "%v: expected literal")
	}
	return ident
}

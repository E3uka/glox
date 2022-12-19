package ast

import (
	"glox/token"
)

// learned from go source
// there are three types of nodes: expressions & type nodes, statement nodes and
// declaration nodes.

type Node interface{}

type Expr interface {
	Node
	expr_node()
}

type Stmt interface {
	Node
	stmt_node()
}

type Decl interface {
	Node
	decl_node()
}

// Expression types
type (
	BinaryExpr struct {
		Lhs      Expr
		Operator token.TOKEN_TYPE
		Rhs      Expr
	}

	IdentExpr struct {
		Name interface{}
		Obj  *Object
	}

	LiteralExpr struct {
		Kind  token.TOKEN_TYPE
		Value any
	}

	ParenExpr struct {
		Expr Expr
	}

	UnaryExpr struct {
		Operator token.TOKEN_TYPE
		Rhs      Expr
	}
)

// ensures only expr_node types can be assigned to Expr
func (*BinaryExpr) expr_node()  {}
func (*IdentExpr) expr_node()   {}
func (*LiteralExpr) expr_node() {}
func (*ParenExpr) expr_node()   {}
func (*UnaryExpr) expr_node()   {}

type (
	AssignStmt struct {
		Lhs   []Expr
		Token token.TOKEN_TYPE
		Rhs   []Expr
	}

	BlockStmt struct {
		List []Stmt
	}

	DeclStmt struct {
		Decl Decl
	}

	ExprStmt struct {
		expr Expr
	}

	ReturnStmt struct {
		Results []Expr
	}
)

// ensures only stmt_node types can be assigned to Stmt
func (*AssignStmt) stmt_node() {}
func (*BlockStmt) stmt_node()  {}
func (*DeclStmt) stmt_node()   {}
func (*ExprStmt) stmt_node()   {}
func (*ReturnStmt) stmt_node() {}

type (
	GenericDecl struct {
		Name  IdentExpr
		Tok   token.TOKEN_TYPE
		Value Expr
	}

	FunDecl struct {
		Tok token.TOKEN_TYPE
	}
)

func (*GenericDecl) decl_node() {}
func (*FunDecl) decl_node()     {}

type Object struct {
	Kind ObjKind
	Name string // declared name
	Decl any    // corresponding Field, XxxSpec, FuncDecl, LabeledStmt, AssignStmt, Scope; or nil
	Data any    // object-specific data; or nil
	// explore below later
	// Type any    // placeholder for type information; may be nil
}

type ObjKind uint

const (
	Bad ObjKind = iota // for error handling
	Pkg                // package
	Con                // constant
	Typ                // type
	Var                // variable
	Fun                // function or method
	Lbl                // label
)

/*
// ==============================================================================
//
//	PRINTING PURPOSES ONLY
//
// ==============================================================================
type AstPrinter struct{}

func (a AstPrinter) Print(expr Expr) string {
	return expr.Visit(a).(string)
}

func (a AstPrinter) VisitBinaryExpr(expr BinaryExpr) interface{} {
	return a.parenthesize(expr.Operator.String(), expr.Lhs, expr.Rhs)
}

func (a AstPrinter) VisitGroupingExpr(expr ParenExpr) interface{} {
	return a.parenthesize("group", expr.Expression)
}

func (a AstPrinter) VisitLiteralExpr(expr LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}
	return fmt.Sprint(expr.Value)
}

func (a AstPrinter) VisitUnaryExpr(expr UnaryExpr) interface{} {
	return a.parenthesize(expr.Operator.String(), expr.Rhs)
}

func (a AstPrinter) VisitStatementExpr(expr StatementExpr) interface{} {
	return a.parenthesize(expr.Ident.(string)+" =", expr.Rhs)
}

// recursively traverses the tree by taking as argument a variadic list of
// expressions that themselves call the parenthesize method by dispatch.
func (a AstPrinter) parenthesize(name string, exprs ...Expr) string {
	var str string
	str += "(" + name
	for _, expr := range exprs {
		// recursively print the expr in the tree
		str += " " + a.Print(expr)
	}
	str += ")"
	return str
}
*/

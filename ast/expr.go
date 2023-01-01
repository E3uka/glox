package ast

import (
	"glox/token"
)

// there exists types of nodes: expressions & type nodes, statement nodes and
// declaration nodes - gleaned from go source
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

type (
	BinaryExpr struct {
		Lhs      Expr
		Operator token.TokenType
		Rhs      Expr
	}
	CallExpr struct {
		Ident *IdentExpr
		Args  []Expr
	}
	CastExpr struct {/* TODO */}
	IdentExpr struct {
		Obj     *Object
		Mutable bool
	}
	LiteralExpr struct {
		Type  Typ
		Value string
	}
	ParenExpr struct {
		Expr Expr
	}
	PtrExpr struct {
		Ident *IdentExpr
		Deref bool
	}
	SelectorExpr struct {
		Parent    *IdentExpr
		Selection Expr
	}
	UnaryExpr struct {
		Operator token.TokenType
		Rhs      Expr
	}
)

// ensures only expr_node types can be assigned to Expr
func (*BinaryExpr)  expr_node() {}
func (*IdentExpr)   expr_node() {}
func (*CallExpr)    expr_node() {}
func (*LiteralExpr) expr_node() {}
func (*ParenExpr)   expr_node() {}
func (*PtrExpr)     expr_node() {}
func (*UnaryExpr)   expr_node() {}

type (
	AssignStmt struct {
		Ident *IdentExpr
	}
	BlockStmt struct {
		List []Stmt
	}
	BranchStmt struct {/* TODO */}
	DeclStmt struct {
		Decl Decl
	}
	EmptyStmt struct {}
	ExprStmt struct { 
		Expr Expr
	}
	ReturnStmt struct {
		Result Expr
	}
)

// ensures only stmt_node types can be assigned to Stmt
func (*AssignStmt) stmt_node() {}
func (*BlockStmt)  stmt_node() {}
func (*BranchStmt) stmt_node() {}
func (*DeclStmt)   stmt_node() {}
func (*EmptyStmt)  stmt_node() {}
func (*ExprStmt)   stmt_node() {}
func (*ReturnStmt) stmt_node() {}

type (
	BasicDecl struct {
		Ident *IdentExpr
		Value Expr
	}

	ProcedureDecl struct {
		Ident *IdentExpr
		Body  *BlockStmt
	}
)

// ensures only decl_node types can be assigned to Decl
func (*BasicDecl)   decl_node() {}
func (*ProcedureDecl) decl_node() {}

type Object struct {
	Kind ObjKind
	Name string
	Decl any // Field, FuncDecl, AssignStmt, Scope; or nil
	Data any // Expr, object-specific data; or nil
	Type any // may be nil
}

type ObjKind uint

const (
	Constant ObjKind = iota
	Procedure
	Type
	Variable
)

type Typ uint

const (
	BoolType Typ = iota
	FloatType
	NullType
	StringType
)

type Scope struct {
	Env *Environment
	Parent *Scope
}

type Environment map[string][]*Object

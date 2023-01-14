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
	BinOp struct {
		Lhs      Expr
		Operator token.TokenType
		Rhs      Expr
	}
	CallExpr struct {
		Ident *Ident
		Args  []Expr
	}
	CastExpr struct {
		To     Typ
		From   Expr
	}
	Ident struct {
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
		Ident *Ident
		Deref bool
	}
	SelectorExpr struct {
		Parent    *Ident
		Selection Expr
	}
	UnOp struct {
		Operator token.TokenType
		Rhs      Expr
	}

	StructType struct {
		Fields []*Field
	}
)

// ensures only expr_node types can be assigned to Expr
func (*BinOp)        expr_node() {}
func (*Ident)        expr_node() {}
func (*CallExpr)     expr_node() {}
func (*CastExpr)     expr_node() {}
func (*LiteralExpr)  expr_node() {}
func (*ParenExpr)    expr_node() {}
func (*PtrExpr)      expr_node() {}
func (*SelectorExpr) expr_node() {}
func (*StructType)   expr_node() {}
func (*UnOp)         expr_node() {}

type (
	AssignStmt struct {
		Ident *Ident
	}
	BlockStmt struct {
		List []Stmt
	}
	BranchStmt struct {
		Token token.TokenType
	}
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
		Ident *Ident
		Value Expr
	}

	ProcedureDecl struct {
		Ident *Ident
		Body  *BlockStmt
	}
)

// ensures only decl_node types can be assigned to Decl
func (*BasicDecl)     decl_node() {}
func (*ProcedureDecl) decl_node() {}

type Field struct {
	Names []*Ident
}

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
	NullType
	F64Type
	S64Type
	StringType
	Custom
)

type Scope struct {
	Env *Environment
	Parent *Scope
}

type Environment map[string][]*Object

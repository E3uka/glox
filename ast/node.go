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
		To   ObjectType
		From Expr
	}
	Ident struct {
		Obj *Object
	}
	LiteralExpr struct {
		Type  ObjectType
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
		Parent    Expr
		Selection Expr
	}
	UnOp struct {
		Operator token.TokenType
		Rhs      Expr
	}
	ProcType struct {/* TODO */}
	IfaceType struct {
		Methods *Fields
	}
	StructType struct {
		Fields *Fields
	}
	Fields struct {
		Names []*Ident
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
func (*IfaceType)    expr_node() {}
func (*StructType)   expr_node() {}
func (*UnOp)         expr_node() {}

type (
	AssignStmt struct {
		Lhs Expr
		Rhs Expr
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
	ExprStmt struct {
		Expr Expr
	}
	IfStmt struct {
		Predicate Expr
		Body      *BlockStmt
		Else      Stmt // can be null
	}
	ReturnStmt struct {
		Result Expr
	}
	WhileStmt struct {
		Predicate Expr
		Body      *BlockStmt
	}
)

// ensures only stmt_node types can be assigned to Stmt
func (*AssignStmt) stmt_node() {}
func (*BlockStmt)  stmt_node() {}
func (*BranchStmt) stmt_node() {}
func (*DeclStmt)   stmt_node() {}
func (*ExprStmt)   stmt_node() {}
func (*IfStmt)     stmt_node() {}
func (*ReturnStmt) stmt_node() {}
func (*WhileStmt)  stmt_node() {}

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
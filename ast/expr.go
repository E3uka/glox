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
	Binary_Expr struct {
		Lhs      Expr
		Operator token.TOKEN_TYPE
		Rhs      Expr
	}

	Ident_Expr struct {
		Obj     *Object
		Mutable bool
	}

	Literal_Expr struct {
		Kind  token.TOKEN_TYPE
		Value string
	}

	Paren_Expr struct {
		Expr Expr
	}

	Ptr_Expr struct {
		Ident *Ident_Expr
		Deref bool
	}

	Unary_Expr struct {
		Operator token.TOKEN_TYPE
		Rhs      Expr
	}
)

// ensures only expr_node types can be assigned to Expr
func (*Binary_Expr)  expr_node() {}
func (*Ident_Expr)   expr_node() {}
func (*Literal_Expr) expr_node() {}
func (*Paren_Expr)   expr_node() {}
func (*Ptr_Expr)     expr_node() {}
func (*Unary_Expr)   expr_node() {}

type (
	Assign_Stmt struct {
		Ident *Ident_Expr
	}

	Block_Stmt struct {
		List []Stmt
	}

	Branch_Stmt struct {} // TODO

	Decl_Stmt struct {
		Decl Decl
	}
	
	Empty_Stmt struct {} // TODO

	Return_Stmt struct {
		Result Expr
	}
)

// ensures only stmt_node types can be assigned to Stmt
func (*Assign_Stmt) stmt_node() {}
func (*Block_Stmt)  stmt_node() {}
func (*Branch_Stmt) stmt_node() {}
func (*Decl_Stmt)   stmt_node() {}
func (*Empty_Stmt)  stmt_node() {}
func (*Return_Stmt) stmt_node() {}

type (
	Generic_Decl struct {
		Ident *Ident_Expr
		Value Expr
	}

	Fun_Decl struct {
		Ident *Ident_Expr
		Body  *Block_Stmt
	}
)

// ensures only decl_node types can be assigned to Decl
func (*Generic_Decl) decl_node() {}
func (*Fun_Decl)     decl_node() {}

type Object struct {
	Kind ObjKind
	Name string
	Decl any // Field, FuncDecl, AssignStmt, Scope; or nil
	Data any // Expr, object-specific data; or nil
	/* TODO:  Type any // placeholder for type information; may be nil */
}

type ObjKind uint

const (
	Constant ObjKind = iota
	Variable
	Function
)

type Scope struct {
	Env *Environment
	Parent *Scope
}

type Environment map[string][]*Object

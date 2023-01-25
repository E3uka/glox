package ast

import "fmt"

// gleaned from source - a visitors Visit method is invoked for each node
// encountered by Walk
type Visitor interface {
	Visit(node Node) (Visitor)
}

/* Helper methods for tree walker */

func WalkIdentList(v Visitor, list []*Ident) {
	for _, i := range list { Walk(v, i) }
}

func WalkExprList(v Visitor, list []Expr) {
	for _, i := range list { Walk(v, i) }
}

func WalkStmtList(v Visitor, list []Stmt) {
	for _, i := range list { Walk(v, i) }
}

func WalkDeclList(v Visitor, list []Decl) {
	for _, i := range list { Walk(v, i) }
}

// walks the abstract syntax tree visiting each node
func Walk(v Visitor, node Node) {
	if v = v.Visit(node); v == nil { return }

	// walk the ast visiting nodes that require further coercion

	switch n := node.(type) {
	/* Fields */
	case *Fields:
		WalkIdentList(v, n.Names)

	/* Expressions */
	case *Ident, *LiteralExpr:
		// do nothing
	case *BinOp:
		Walk(v, n.Lhs)
		Walk(v, n.Rhs)
	case *CallExpr:
		Walk(v, n.Ident)
		WalkExprList(v, n.Args)
	case *CastExpr:
		Walk(v, n.From)
	case *ParenExpr:
		Walk(v, n.Expr)
	case *PtrExpr:
		Walk(v, n.Ident)
	case *SelectorExpr:
		Walk(v, n.Parent)
		Walk(v, n.Selection)
	case *UnOp:
		Walk(v, n.Rhs)
	case *IfaceType:
		Walk(v, n.Methods)
	case *StructType:
		Walk(v, n.Fields)

	/* Statements */
	case *BranchStmt:
		// do nothing
	case *AssignStmt:
		Walk(v, n.Lhs)
		Walk(v, n.Rhs)
	case *BlockStmt:
		WalkStmtList(v, n.List)
	case *DeclStmt:
		Walk(v, n.Decl)
	case *ExprStmt:
		Walk(v, n.Expr)
	case *IfStmt:
		Walk(v, n.Predicate)
		Walk(v, n.Body)
		if n.Else != nil { Walk(v, n.Else) }
	case *ReturnStmt:
		Walk(v, n.Result)
	case *WhileStmt:
		Walk(v, n.Predicate)
		Walk(v, n.Body)

	/* Declarations */
	case *BasicDecl:
		Walk(v, n.Ident)
		Walk(v, n.Value)
	case *ProcedureDecl:
		Walk(v, n.Ident)
		Walk(v, n.Body)

	default:
		panic(fmt.Sprintf("tree walker: unexpected node %T", n))
	}

	v.Visit(nil)
}
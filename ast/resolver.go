package ast

import (
	"fmt"
	"go/ast"
)

type resolver struct {
	local     *Scope
	procedure *Scope
	// unresolved []*Ident
}

func scope_resolver() *resolver {
	local_scope := new_scope(nil)
	proc_scope := new_scope(nil)
	return &resolver{local: local_scope, procedure: proc_scope}
}

func new_object_from(ident *Ident) *Object {
	return &Object{
		Kind:    ident.Obj.Kind,
		Name:    ident.Obj.Name,
		Mutable: ident.Obj.Mutable,
	}
}

// associates a ident with its subsequent data
func (r *resolver) declare(ident *Ident, data, decl any) {
	if ident.Obj.Data != nil {
		panic("already declared identifier")
	}
	obj := new_object_from(ident)
	obj.Data = data
	obj.Decl = decl
	if _, ok := decl.(*ast.Ident); !ok {
		ident.Obj = obj
	}

	if ins := r.local.Insert(obj); ins == obj {
		panic(fmt.Sprintf("already inserted %v\n", ins))
	}
}

func (r *resolver) resolve_ident(ident *Ident) {
	for s := r.local; s != nil; s = s.Parent {
		s.Find(ident.Obj.Name)
	}
}

// resolves identifiers into an environment scope for lookup
func (r *resolver) Visit(node Node) Visitor {
	switch n := node.(type) {
	/* Fields */
	case *Fields:
		WalkIdentList(r, n.Names)

	/* EXPRESSIONS */
	case *Ident:
		r.resolve_ident(n)
	case *CastExpr:
		Walk(r, n.From)
	case *ParenExpr:
		Walk(r, n.Expr)
	case *PtrExpr:
		Walk(r, n.Ident)
	case *SelectorExpr:
		Walk(r, n.Parent)
		// Walk(r, n.Selection)
	case *UnOp:
		Walk(r, n.Rhs)
	case *IfaceType:
		Walk(r, n.Methods)
	case *StructType:
		Walk(r, n.Fields)

	/* STATEMENTS */
	case *AssignStmt:
		Walk(r, n.Lhs)
		Walk(r, n.Rhs)
	case *BlockStmt:
		WalkStmtList(r, n.List)
	case *DeclStmt:
		Walk(r, n.Decl)
	case *ExprStmt:
		Walk(r, n.Expr)
	case *IfStmt:
		Walk(r, n.Predicate)
		Walk(r, n.Body)
		if n.Else != nil {
			Walk(r, n.Else)
		}
	case *ReturnStmt:
		Walk(r, n.Result)
	case *WhileStmt:
		Walk(r, n.Predicate)
		Walk(r, n.Body)

	/* DECLARATIONS */
	case *BasicDecl:
		r.declare(n.Ident, nil, n.Value)
		Walk(r, n.Value)
	case *ProcedureDecl:
		r.declare(n.Ident, nil, n.Body)
		Walk(r, n.Body)

	default:
		return r
	}
	return r.Visit(nil)
}

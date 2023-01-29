package ast

import "fmt"

type resolver struct {
	local     *Scope
	unresolved []*Ident
}

func scope_resolver() *resolver {
	local := new_scope(nil)
	return &resolver{local: local}
}

func new_object_from(ident *Ident) *Object {
	return &Object{
		Kind:    ident.Obj.Kind,
		Name:    ident.Obj.Name,
		Mutable: ident.Obj.Mutable,
	}
}

func (r *resolver) declare(ident *Ident, data, decl any) {
	if ident.Obj.Data != nil { panic("already declared identifier") }
	obj := new_object_from(ident)
	obj.Data = data
	obj.Decl = decl
	if _, ok := decl.(*Ident); !ok { ident.Obj = obj }
	// TODO: if the ident is a procedure this should bubble up and live at
	// the 'highest' parent scope.
	if ins := r.local.Insert(obj); ins == obj {
		panic(fmt.Sprintf("already inserted %v\n", ins))
	}
}

func (r *resolver) resolve_ident(ident *Ident) {
	for cur_scope := r.local; cur_scope != nil; cur_scope = cur_scope.Parent {
		if item := cur_scope.Find(ident.Obj.Name); item != nil {
			return // found declaration in either local or parent scope
		}
	}
	marked_unresolved := false
	for _, entry := range r.unresolved {
		if entry == ident { marked_unresolved = true}
	}
	if !marked_unresolved { r.unresolved = append(r.unresolved, ident) }
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

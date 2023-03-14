package ast

import "fmt"

type Scope struct {
	Local  Environment
	Parent *Scope
}

type Environment map[string]*Object

func new_scope(parent *Scope) *Scope {
	return &Scope{Local: make(Environment, 4), Parent: parent}
}

func (s *Scope) Find(name string) *Object { return s.Local[name] }

func (env Environment) Inject(obj *Object) (return_obj *Object) {
	if return_obj = env[obj.Name]; return_obj == nil {
		env[obj.Name] = obj
	}
	return
}

type scope_walker struct {
	scope      *Scope
	declared   map[string]*Object
	unresolved []*Object
}

func sc_walker() *scope_walker {
	parent := new_scope(nil)
	local := new_scope(parent)
	return &scope_walker{
		scope: local,
		declared: make(map[string]*Object),
		unresolved: []*Object{},
	}
}

func new_object_from(ident *Ident) *Object {
	return &Object{
		Kind:    ident.Obj.Kind,
		Name:    ident.Obj.Name,
		Mutable: ident.Obj.Mutable,
		Decl:    ident.Obj.Decl,
		Data:    nil,
	}
}

func (sw *scope_walker) declare(ident *Ident, data, decl any) {
	if ident.Obj.Data != nil { panic("already declared identifier") }
	obj := new_object_from(ident)
	if ident.Obj.Kind == Procedure {
		if ins := sw.scope.Parent.Local.Inject(obj); ins == obj {
			panic(fmt.Sprintf("already inserted %v\n", ins))
		}
	} else {
		if ins := sw.scope.Local.Inject(obj); ins == obj {
			panic(fmt.Sprintf("already inserted %v\n", ins))
		}
	}
	sw.declared[ident.Obj.Name] = ident.Obj
}

func (sw *scope_walker) resolve_ident(ident *Ident) {
	found_in_scope := false;
	for cur_scope := sw.scope; cur_scope != nil; cur_scope = cur_scope.Parent {
	    // two objects may share the same name, i.e. procedure and its method call
		// but they may have differnt underlying data
		if obj := cur_scope.Find(ident.Obj.Name); obj == ident.Obj {
			found_in_scope = true
			break
		}
	}
	if _, exists := sw.declared[ident.Obj.Name]; !exists && !found_in_scope {
		sw.unresolved = append(sw.unresolved, ident.Obj)
	}
}

// resolves identifiers into an environment scope for lookup
func (sw *scope_walker) Visit(node Node) Visitor {
	switch n := node.(type) {
	/* Fields */
	case *Fields:
		WalkIdentList(sw, n.Names)

	/* EXPRESSIONS */
	case *Ident:
		sw.resolve_ident(n)
	case *CastExpr:
		Walk(sw, n.From)
	case *ParenExpr:
		Walk(sw, n.Expr)
	case *PtrExpr:
		Walk(sw, n.Ident)
	case *SelectorExpr:
		Walk(sw, n.Parent)
		// Walk(sw, n.Selection)
	case *UnOp:
		Walk(sw, n.Rhs)
	case *IfaceType:
		for _, f := range n.Methods.Names { sw.declare(f, nil, nil) }
	case *StructType:
		Walk(sw, n.Fields)

	/* STATEMENTS */
	case *AssignStmt:
		Walk(sw, n.Lhs)
		Walk(sw, n.Rhs)
	case *BlockStmt:
		WalkStmtList(sw, n.List)
	case *DeclStmt:
		Walk(sw, n.Decl)
	case *ExprStmt:
		Walk(sw, n.Expr)
	case *IfStmt:
		Walk(sw, n.Predicate)
		Walk(sw, n.Body)
		if n.Else != nil { Walk(sw, n.Else) }
	case *ReturnStmt:
		Walk(sw, n.Result)
	case *WhileStmt:
		Walk(sw, n.Predicate)
		Walk(sw, n.Body)

	/* DECLARATIONS */
	case *BasicDecl:
		sw.declare(n.Ident, nil, n.Value)
		Walk(sw, n.Value)
	case *ProcedureDecl:
		sw.declare(n.Ident, nil, n.Body)
		Walk(sw, n.Body)

	default:
		return sw
	}
	return sw.Visit(nil)
}
package ast

type ObjKind uint

const (
	Constant ObjKind = iota
	Procedure
	Call
	Type
	Variable
	Interface
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

type Object struct {
	Kind ObjKind
	Name string
	Mutable bool
	Decl any // Field, FuncDecl, AssignStmt, Environment; or nil
	Data any // Object-specific data or nil; added during resolution pass
	Type Typ // may be set, nil; or later inferred
}
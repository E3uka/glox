package ast

type ObjectKind int

const (
	Constant ObjectKind = iota
	Procedure
	Call
	Type
	Variable
	Interface
)

type ObjectType int

const (
	BoolType ObjectType = iota
	NullType
	F64Type
	S64Type
	StringType
	Custom
)

type Object struct {
	Name    string
	Kind    ObjectKind
	Type    ObjectType // may be set, nil; or later inferred
	Mutable bool
	Decl    any        // Field, FuncDecl, AssignStmt, Environment; or nil
	Data    any        // Object-specific data or nil; added during resolution pass
}
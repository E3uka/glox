package ast

import (
	"encoding/json"
	"fmt"
)

type interpreter struct {
	path  *string
	nodes []Node
}

func New(path *string, nodes []Node) *interpreter {
	return &interpreter{path: path, nodes: nodes}
}

// Walks the abstract syntax tree mutliple times each time resolving
// individual pieces needed to build-up and evaluate the program.
func (i *interpreter) Interpret() {
	scopes := sc_walker()
	// types := type_resolver()

	// invoke the various walker methods
	for _, n := range i.nodes {
		Walk(scopes, n)
		// Walk(types, n)
	}
	for _, item := range scopes.scope.Local {
		local_scope, _ := json.Marshal(item)
		fmt.Printf("local scope: %s\n\n", local_scope)
	}
	for _, item := range scopes.scope.Parent.Local {
		parent_scope, _ := json.Marshal(item)
		fmt.Printf("parent scope: %s\n\n", parent_scope)
	}
	for _, item := range scopes.unresolved {
		json, _ := json.Marshal(item.Obj)
		fmt.Printf("unresolved: %s\n\n", json)
	}

	// TODO: compile to LLVM IR
}
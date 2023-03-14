package ast

import (
	"encoding/json"
	"fmt"
)

type interpreter struct {
	path  *string
	tree  []Node
}

func New(path *string, nodes []Node) *interpreter {
	return &interpreter{path: path, tree: nodes}
}

func (i *interpreter) Interpret() {
	fmt.Println()
	scopes := sc_walker()
	// types := type_resolver()
	for _, node := range i.tree {
		Walk(scopes, node)
		// Walk(types, node)
	}
	for _, item := range scopes.scope.Local {
		local_scope, _ := json.Marshal(item)
		fmt.Printf("local-scope: %s\n", local_scope)
	}
	fmt.Println()
	for _, item := range scopes.scope.Parent.Local {
		parent_scope, _ := json.Marshal(item)
		fmt.Printf("parent-scope: %s\n", parent_scope)
	}
	fmt.Println()
	for _, item := range scopes.unresolved {
		json, _ := json.Marshal(item)
		fmt.Printf("unresolved: %s\n", json)
	}
	fmt.Println()

	// TODO: compile to LLVM IR
}

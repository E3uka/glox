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
	scope_walker := sc_walker()
	for _, node := range i.tree {
		Walk(scope_walker, node)
	}
	for _, item := range scope_walker.scope.Local {
		local_scope, _ := json.Marshal(item)
		fmt.Printf("local-scope: %s\n", local_scope)
	}
	fmt.Println()
	for _, item := range scope_walker.scope.Parent.Local {
		parent_scope, _ := json.Marshal(item)
		fmt.Printf("parent-scope: %s\n", parent_scope)
	}
	fmt.Println()
	for _, item := range scope_walker.unresolved {
		json, _ := json.Marshal(item)
		fmt.Printf("unresolved: %s\n", json)
	}
	fmt.Println()
	// TODO: compile to LLVM IR
}
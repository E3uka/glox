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
	scopes := scope_resolver()
	// types := type_resolver()

	for _, n := range i.nodes {
		Walk(scopes, n)
		// Walk(types, n)
	}
	for _, val := range scopes.local.Local {
		json, _ := json.Marshal(val)
		fmt.Printf("%s\n\n", json)
	}

	// eval(scopes, types)
}
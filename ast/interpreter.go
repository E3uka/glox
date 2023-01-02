package ast

type interpreter struct {
	path      *string
	prog_stmt map[interface{}]interface{}
	mutable   map[interface{}]bool
}

func New(path *string) *interpreter {
	return &interpreter{
		path:      path,
		prog_stmt: make(map[interface{}]interface{}),
		mutable:   make(map[interface{}]bool),
	}
}

func (i *interpreter) Interpret() {}

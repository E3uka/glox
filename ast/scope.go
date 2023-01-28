package ast

type Scope struct {
	Local  Environment
	Parent *Scope
}

type Environment map[string]*Object

func new_scope(parent *Scope) *Scope {
	return &Scope{Local: Environment{}, Parent: parent}
}

func (s *Scope) Find(name string) *Object { return s.Local[name] }

func (s *Scope) Insert(obj *Object) (return_obj *Object) {
	if return_obj = s.Local[obj.Name]; return_obj == nil {
		s.Local[obj.Name] = obj
	}
	return
}
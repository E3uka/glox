package ast

import (
	"glox/token"
	"reflect"
)

type interpreter struct {
	ast Expr
}

func NewInterpreter(ast Expr) *interpreter {
	return &interpreter{ast}
}

func (i *interpreter) Interpret() interface{} {
	value := i.ast.Evaluate(i)
	return value
}

func (i *interpreter) VisitBinaryExpr(expr BinaryExpr) interface{} {
	lhs := expr.Lhs.Evaluate(i)
	rhs := expr.Rhs.Evaluate(i)
	switch expr.Operator {
	case token.SUB:
		return lhs.(float64) - rhs.(float64)
	case token.ADD:
		if instance_of_float(lhs) && instance_of_float(rhs) {
			return lhs.(float64) + rhs.(float64)
		}
		if instance_of_string(lhs) && instance_of_string(rhs) {
			return lhs.(string) + rhs.(string)
		}
	case token.MUL:
		return lhs.(float64) * rhs.(float64)
	case token.QUO:
		return lhs.(float64) / rhs.(float64)

	case token.INCRBY:
		return lhs.(float64) + rhs.(float64)
	case token.DECRYBY:
		return lhs.(float64) - rhs.(float64)

	case token.GTR:
		return lhs.(float64) > rhs.(float64)
	case token.GEQ:
		return lhs.(float64) >= rhs.(float64)
	case token.LSS:
		return lhs.(float64) < rhs.(float64)
	case token.LEQ:
		return lhs.(float64) <= rhs.(float64)
	case token.EQL:
		return is_equal(lhs, rhs)
	case token.NEQ:
		return !is_equal(lhs, rhs)
	}
	return nil // unreachable
}

func (i *interpreter) VisitGroupingExpr(expr GroupingExpr) interface{} {
	return expr.Expression.Evaluate(i)
}

func (i *interpreter) VisitLiteralExpr(expr LiteralExpr) interface{} {
	return expr.Value

}
func (i *interpreter) VisitUnaryExpr(expr UnaryExpr) interface{} {
	rhs := expr.Rhs.Evaluate(i)
	switch expr.Operator {
	case token.SUB:
		return -rhs.(float64)
	case token.NOT:
		return !is_truth(rhs)

	case token.INCR:
		return rhs.(float64) + 1
	case token.DECR:
		return rhs.(float64) - 1
	default:
		return nil // unreachable
	}
}

func is_truth(expr interface{}) bool {
	if expr == nil {
		return false
	}
	if found_bool, ok := expr.(bool); ok {
		return found_bool
	}
	return true
}

func instance_of_string(expr interface{}) bool {
	_, ok := expr.(string)
	return ok
}

func instance_of_float(expr interface{}) bool {
	_, ok := expr.(float64)
	return ok
}

func is_equal(left, right interface{}) bool {
	if left == nil && right == nil {
		return true
	}
	if left == nil {
		return false
	}
	return reflect.DeepEqual(left, right)
}

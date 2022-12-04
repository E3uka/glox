package ast

import (
	"glox/token"
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
	case token.INCR:
		return rhs.(float64) + 1
	case token.DECR:
		return rhs.(float64) - 1
	default:
		return nil // unreachable
	}
}

func instance_of_string(expr interface{}) bool {
	_, ok := expr.(string)
	return ok
}

func instance_of_float(expr interface{}) bool {
	_, ok := expr.(float64)
	return ok
}

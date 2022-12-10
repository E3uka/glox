package ast

import (
	"fmt"
	gloxError "glox/error"
	"glox/token"
	"reflect"
)

type interpreter struct {
	path *string
	ast  Expr
	stmt map[string]interface{}
}

func NewInterpreter(path *string, ast Expr) *interpreter {
	return &interpreter{
		path: path,
		ast:  ast,
		stmt: make(map[string]interface{}),
	}
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
		i.check_float_operands(expr.Operator, lhs, rhs)
		return lhs.(float64) - rhs.(float64)
	case token.ADD:
		if instance_of_float(lhs) && instance_of_float(rhs) {
			return lhs.(float64) + rhs.(float64)
		}
		if instance_of_string(lhs) && instance_of_string(rhs) {
			return lhs.(string) + rhs.(string)
		}
		gloxError.RuntimeError(
			i.path,
			"operands must both be either float64 or string",
			lhs,
			rhs,
		)
	case token.MUL:
		i.check_float_operands(expr.Operator, lhs, rhs)
		return lhs.(float64) * rhs.(float64)
	case token.QUO:
		i.check_float_operands(expr.Operator, lhs, rhs)
		return lhs.(float64) / rhs.(float64)

	case token.INCRBY:
		return lhs.(float64) + rhs.(float64)
	case token.DECRYBY:
		return lhs.(float64) - rhs.(float64)

	case token.GTR:
		i.check_float_operands(expr.Operator, lhs, rhs)
		return lhs.(float64) > rhs.(float64)
	case token.GEQ:
		i.check_float_operands(expr.Operator, lhs, rhs)
		return lhs.(float64) >= rhs.(float64)
	case token.LSS:
		i.check_float_operands(expr.Operator, lhs, rhs)
		return lhs.(float64) < rhs.(float64)
	case token.LEQ:
		i.check_float_operands(expr.Operator, lhs, rhs)
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
		i.check_float_operand(expr.Operator, rhs)
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

func (i *interpreter) VisitStatementExpr(expr StatementExpr) interface{} {
	name := expr.Ident.(string)
	rhs := expr.Rhs.Evaluate(i)
	i.stmt[name] = rhs // add to identifier lookup table
	return fmt.Sprintf("%v = %v", name, rhs)
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
	// lean on go implementation of equality
	return reflect.DeepEqual(left, right)
}

func (i *interpreter) check_float_operand(
	operator token.TOKEN_TYPE,
	operand interface{},
) {
	if _, ok := operand.(float64); ok {
		return
	}
	gloxError.RuntimeError(i.path, "operand must be float64", operand)
}

func (i *interpreter) check_float_operands(
	operator token.TOKEN_TYPE,
	left interface{},
	right interface{},
) {
	_, is_left_float := left.(float64)
	_, is_right_float := right.(float64)

	if is_left_float && is_right_float {
		return
	}
	gloxError.RuntimeError(
		i.path,
		"operands must both be float64",
		left,
		right,
	)
}

package ast

import (
	"fmt"
	gloxError "glox/error"
	"glox/token"
	"reflect"
)

type interpreter struct {
	path       *string
	stmt_exprs []StatementExpr
	prog_stmt  map[interface{}]interface{}
	mutable    map[interface{}]bool
}

func New(path *string, stmt_exprs []StatementExpr) *interpreter {
	return &interpreter{
		path:       path,
		stmt_exprs: stmt_exprs,
		prog_stmt:  make(map[interface{}]interface{}),
		mutable:    make(map[interface{}]bool),
	}
}

func (i *interpreter) Interpret() {
	for _, stmt := range i.stmt_exprs {
		// short-circuiting panic recovery, raises error and exits
		defer func() {
			if r := recover(); r != nil {
				gloxError.Runtime_Panic_Recover(fmt.Sprint(r))
			}
		}()

		// handle identifier occurences and mutability constraints
		if _, exists := i.prog_stmt[stmt.Ident]; !exists {
			if stmt.Reassignment == true {
				if _, exists := i.mutable[stmt.Ident]; !exists {
					gloxError.Runtime_Panic(
						i.path,
						"cannot find value in this scope",
						stmt.Ident,
					)
				}
			}
			i.mutable[stmt.Ident] = stmt.Mutable
			value := stmt.Rhs.Evaluate(i)
			i.prog_stmt[stmt.Ident] = value
			fmt.Printf("%v = %v\n", stmt.Ident, value)
		} else {
			if !i.mutable[stmt.Ident] {
				gloxError.Runtime_Panic(
					i.path,
					"cannot assign twice to an immutable variable",
					stmt.Ident,
				)
			}
			value := stmt.Rhs.Evaluate(i)
			i.prog_stmt[stmt.Ident] = value
			fmt.Printf("%v = %v\n", stmt.Ident, value)
		}
	}
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
		gloxError.Runtime_Panic(
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

func (i *interpreter) VisitStatementExpr(expr StatementExpr) interface{} {
	expr.Rhs.Evaluate(i)
	return nil
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

func (i *interpreter) VisitVariableExpr(expr VariableExpr) interface{} {
	value, found := i.prog_stmt[expr.Ident.Evaluate(i)]
	if !found {
		gloxError.Runtime_Panic(
			i.path,
			"cannot find value in this scope",
			expr.Ident.Evaluate(i),
		)
	}
	return value
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
	gloxError.Runtime_Panic(i.path, "operand must be float64", operand)
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
	gloxError.Runtime_Panic(
		i.path,
		"operands must both be float64",
		left,
		right,
	)
}

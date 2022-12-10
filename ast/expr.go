package ast

import (
	"fmt"
	"glox/token"
)

type Expr interface {
	Evaluate(visitor ExprVisitor) interface{}
}

type BinaryExpr struct {
	Lhs      Expr
	Operator token.TOKEN_TYPE
	Rhs      Expr
}

func (b BinaryExpr) Evaluate(visitor ExprVisitor) interface{} {
	return visitor.VisitBinaryExpr(b)
}

type GroupingExpr struct {
	Expression Expr
}

// a group uses parentheses to control execution order
func (g GroupingExpr) Evaluate(visitor ExprVisitor) interface{} {
	return visitor.VisitGroupingExpr(g)
}

type LiteralExpr struct {
	Value interface{}
}

func (l LiteralExpr) Evaluate(visitor ExprVisitor) interface{} {
	return visitor.VisitLiteralExpr(l)
}

type UnaryExpr struct {
	Operator token.TOKEN_TYPE
	Rhs      Expr
}

func (u UnaryExpr) Evaluate(visitor ExprVisitor) interface{} {
	return visitor.VisitUnaryExpr(u)
}

type StatementExpr struct {
	Ident interface{}
	Rhs   Expr
}

func (f StatementExpr) Evaluate(visitor ExprVisitor) interface{} {
	return visitor.VisitStatementExpr(f)
}

type VariableExpr struct {
	Ident       interface{}
	Initializer Expr
}

type ExprVisitor interface {
	VisitBinaryExpr(expr BinaryExpr) interface{}
	VisitGroupingExpr(expr GroupingExpr) interface{}
	VisitLiteralExpr(expr LiteralExpr) interface{}
	VisitUnaryExpr(expr UnaryExpr) interface{}
	VisitStatementExpr(expr StatementExpr) interface{}
}

// ==============================================================================
//
//	PRINTING PURPOSES ONLY
//
// ==============================================================================
type AstPrinter struct{}

func (a AstPrinter) Print(expr Expr) string {
	return expr.Evaluate(a).(string)
}

func (a AstPrinter) VisitBinaryExpr(expr BinaryExpr) interface{} {
	return a.parenthesize(expr.Operator.String(), expr.Lhs, expr.Rhs)
}

func (a AstPrinter) VisitGroupingExpr(expr GroupingExpr) interface{} {
	return a.parenthesize("group", expr.Expression)
}

func (a AstPrinter) VisitLiteralExpr(expr LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}
	return fmt.Sprint(expr.Value)
}

func (a AstPrinter) VisitUnaryExpr(expr UnaryExpr) interface{} {
	return a.parenthesize(expr.Operator.String(), expr.Rhs)
}

func (a AstPrinter) VisitStatementExpr(expr StatementExpr) interface{} {
	return a.parenthesize(expr.Ident.(string)+" =", expr.Rhs)
}

// recursively traverses the tree by taking as argument a variadic list of
// expressions that themselves call the parenthesize method by dispatch.
func (a AstPrinter) parenthesize(name string, exprs ...Expr) string {
	var str string
	str += "(" + name
	for _, expr := range exprs {
		// recursively print the expr in the tree
		str += " " + a.Print(expr)
	}
	str += ")"
	return str
}

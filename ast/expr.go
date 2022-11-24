package ast

import (
	"fmt"
	"glox/token"
)

type Expr interface {
	Accept(visitor ExprVisitor) interface{}
}

type AssignExpr struct {
	Name  token.Token
	Value Expr
}

func (a AssignExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitAssignExpr(a)
}

type BinaryExpr struct {
	Lhs      Expr
	Operator token.Token
	Rhs      Expr
}

func (b BinaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitBinaryExpr(b)
}

type GroupingExpr struct {
	Expression Expr
}

// a group uses parentheses to control execution order
func (g GroupingExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitGroupingExpr(g)
}

type LiteralExpr struct {
	Value interface{}
}

func (l LiteralExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitLiteralExpr(l)
}

type UnaryExpr struct {
	Operator token.Token
	Rhs      Expr
}

func (u UnaryExpr) Accept(visitor ExprVisitor) interface{} {
	return visitor.VisitUnaryExpr(u)
}

type ExprVisitor interface {
	VisitAssignExpr(expr AssignExpr) interface{}
	VisitBinaryExpr(expr BinaryExpr) interface{}
	VisitGroupingExpr(expr GroupingExpr) interface{}
	VisitLiteralExpr(expr LiteralExpr) interface{}
	VisitUnaryExpr(expr UnaryExpr) interface{}
}

// ==============================================================================
//
//	PRINTING PURPOSES ONLY
//
// ==============================================================================
type AstPrinter struct{}

func (a AstPrinter) Print(expr Expr) string {
	return expr.Accept(a).(string)
}

func (a AstPrinter) VisitAssignExpr(expr AssignExpr) interface{} {
	return a.parenthesize("= "+expr.Name.Lexeme, expr.Value)
}

func (a AstPrinter) VisitBinaryExpr(expr BinaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Lhs, expr.Rhs)
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
	return a.parenthesize(expr.Operator.Lexeme, expr.Rhs)
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

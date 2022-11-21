package main

import (
	"fmt"
	"glox/ast"
	"glox/token"
)

type astPrinter struct{}

func (a astPrinter) print(expr ast.Expr) string {
	return expr.Accept(a).(string)
}

func (a astPrinter) VisitAssignExpr(expr ast.AssignExpr) interface{} {
	return a.parenthesize("= "+expr.Name.Lexeme, expr.Value)
}

func (a astPrinter) VisitBinaryExpr(expr ast.BinaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Lhs, expr.Rhs)
}

func (a astPrinter) VisitGroupingExpr(expr ast.GroupingExpr) interface{} {
	return a.parenthesize("group", expr.Expression)
}
func (a astPrinter) VisitLiteralExpr(expr ast.LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}
	return fmt.Sprint(expr.Value)
}

func (a astPrinter) VisitUnaryExpr(expr ast.UnaryExpr) interface{} {
	return a.parenthesize(expr.Operator.Lexeme, expr.Rhs)
}

// recursively traverses the tree by taking as argument a variadic list of
// expressions that themselves call the parenthesize method by dispatch.
func (a astPrinter) parenthesize(name string, exprs ...ast.Expr) string {
	var str string
	str += "(" + name
	for _, expr := range exprs {
		// recursively print the expr in the tree
		str += " " + a.print(expr)
	}
	str += ")"
	return str
}

func main() {
	expression := ast.BinaryExpr{
		Lhs: ast.UnaryExpr{
			Operator: token.Token{
				TokType: token.SUB,
				Lexeme:  "-",
				Literal: nil,
				Line:    1,
			},
			Rhs: ast.LiteralExpr{
				Value: 123,
			},
		},
		Operator: token.Token{
			TokType: token.MUL,
			Lexeme:  "*",
			Literal: nil,
			Line:    1,
		},
		Rhs: ast.GroupingExpr{
			Expression: ast.BinaryExpr{
				Lhs: ast.LiteralExpr{
					Value: 28,
				},
				Operator: token.Token{
					TokType: token.ADD,
					Lexeme:  "+",
					Literal: nil,
					Line:    1,
				},
				Rhs: ast.LiteralExpr{
					Value: 45.67,
				},
			},
		},
	}
	fmt.Println(astPrinter{}.print(expression))
}

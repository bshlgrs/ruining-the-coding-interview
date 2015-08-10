package java_transpiler.queries

import java_transpiler.JavaExpressionOrQuery

import cas.MathExp

class OrderByClause {

}

case class GoodOrderByClause(nodeExpr: MathExp[JavaExpressionOrQuery]) extends OrderByClause
case class BadOrderByClause(expr: MathExp[JavaExpressionOrQuery]) extends OrderByClause

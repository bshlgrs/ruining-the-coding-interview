package generic_expression_ast

import cas._

sealed abstract class ExpressionAst

case class MathExpressionAst(thing: MathExp[ExpressionAst]) extends ExpressionAst
case class FunctionCall(name: Name, args: List[ExpressionAst]) extends ExpressionAst
case class MethodCall(thing: ExpressionAst, methodName: Name, args: List[ExpressionAst]) extends ExpressionAst
case class Variable(name: Name) extends ExpressionAst

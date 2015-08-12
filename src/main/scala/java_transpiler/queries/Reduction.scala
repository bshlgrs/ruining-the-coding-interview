package java_transpiler.queries

import java_transpiler._

import ast_renderers.RubyOutputter
import cas.MathExp

case class Reduction(start: JavaExpressionOrQuery,
                     mapper: Mapper,
                     reducer: Reducer) {
  lazy val freeVariables: Set[String] = start.freeVariables ++ mapper.freeVariables ++ reducer.freeVariables

  override def toString: String = {
    s"Reduction[${RubyOutputter.outputExpression(start)}, $mapper, $reducer]"
  }

  def modify(modifier: AstModifier): Reduction = {
    Reduction(modifier.applyToExpr(start),
      mapper.copy(body = modifier.applyToExpr(mapper.body)),
      reducer.copy(body = modifier.applyToExpr(reducer.body)))
  }
}

object Reduction {
  def build(start: JavaExpressionOrQuery,
            map: JavaExpressionOrQuery,
            reducer: JavaExpressionOrQuery,
            context: JavaContext) = {
    Reduction(start, buildMapper(map), buildReducer(reducer))
  }

  def buildMapper(map: JavaExpressionOrQuery) = {
    map match {
      case JavaLambdaExpr(List(arg1), body) => Mapper(arg1._1, body)
      case _ => ???
    }
  }

  def buildReducer(map: JavaExpressionOrQuery) = {
    map match {
      case JavaLambdaExpr(List(arg1, arg2), body) => Reducer(arg1._1, arg2._1, body)
      case _ => ???
    }
  }
}

case class Mapper(arg: String, body: JavaExpressionOrQuery) {
  lazy val freeVariables = body.freeVariables - arg

  override def toString: String = s"($arg -> ${RubyOutputter.outputExpression(body)})"

  def asJavaLambda: JavaLambdaExpr = JavaLambdaExpr(List(arg -> JavaIntType), body)
}

case class Reducer(arg1: String, arg2: String, body: JavaExpressionOrQuery) {
  lazy val freeVariables = body.freeVariables - arg1 - arg2

  override def toString: String = s"($arg1, $arg2 -> ${RubyOutputter.outputExpression(body)})"

  def asJavaLambda: JavaLambdaExpr = JavaLambdaExpr(List(arg1 -> JavaIntType, arg2 -> JavaIntType), body)
}



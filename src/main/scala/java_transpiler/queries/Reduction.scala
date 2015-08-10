package java_transpiler.queries

import java_transpiler.{JavaLambdaExpr, JavaExpressionOrQuery, JavaExpression}

import ast_renderers.RubyOutputter
import cas.MathExp

case class Reduction(start: JavaExpressionOrQuery,
                     mapper: Mapper,
                     reducer: Reducer) {
  lazy val freeVariables: Set[String] = start.freeVariables ++ mapper.freeVariables ++ reducer.freeVariables

  def toReasonableString(): String = {
    s"Reduction[${RubyOutputter.outputExpression(start)}, ${mapper.toReasonableString()}, ${reducer.toReasonableString()}]"
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

  def toReasonableString(): String = s"($arg -> ${RubyOutputter.outputExpression(body)})"
}

case class Reducer(arg1: String, arg2: String, body: JavaExpressionOrQuery) {
  lazy val freeVariables = body.freeVariables - arg1 - arg2

  def toReasonableString(): String = s"($arg1, $arg2 -> ${RubyOutputter.outputExpression(body)})"
}



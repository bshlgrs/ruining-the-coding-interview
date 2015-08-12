package java_transpiler.queries

import java_transpiler._

import ast_renderers.RubyOutputter
import cas.{CasVariable, MathExp}
import helpers.VariableNameGenerator

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

  lazy val isInvertible: Boolean = invertedReducer.isDefined

  lazy val invertedReducer: Option[Reducer] = reducer.invert
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

  def useStr(name: String) = body.replaceVariables(Map(arg -> JavaVariable(name)))

  override def toString: String = s"($arg -> ${RubyOutputter.outputExpression(body)})"

  def asJavaLambda: JavaLambdaExpr = JavaLambdaExpr(List(arg -> JavaIntType), body)
}

case class Reducer(arg1: String, arg2: String, body: JavaExpressionOrQuery) {
  lazy val freeVariables = body.freeVariables - arg1 - arg2

  override def toString: String = s"($arg1, $arg2 -> ${RubyOutputter.outputExpression(body)})"

  def asJavaLambda: JavaLambdaExpr = JavaLambdaExpr(List(arg1 -> JavaIntType, arg2 -> JavaIntType), body)

  def useBody(map: Map[String, JavaExpressionOrQuery]): JavaExpressionOrQuery = body.replaceVariables(map)

  def invert: Option[Reducer] = {
    val otherSide = JavaVariable(VariableNameGenerator.getRandomName())

    body match {
      case JavaMath(ast) =>
        (ast - JavaMathHelper.casify(otherSide)).solve(JavaVariable(arg1)) match {
          case Some(solution) => Some(Reducer(arg2, otherSide.name, JavaMathHelper.decasify(solution)))
          case None => None
        }
      case _ => None
    }
  }
}

package java_transpiler.queries

import java_transpiler._

import ast_renderers.RubyOutputter
import cas.{Number, Name}

case class UnorderedQuery(
            source: JavaExpressionOrQuery,
            whereClauses: List[WhereClause],
            limiter: Option[LimitByClause],
            reduction: Option[Reduction]) {

  lazy val targetIsMagic: Boolean = source.isInstanceOf[JavaVariable]

  val parameters = whereClauses.flatMap(_.freeVariables)++ limiter.map(_.freeVariables) ++ reduction.map(_.freeVariables)

  def childrenExpressions(): List[JavaExpressionOrQuery] = {
    whereClauses.flatMap(_.childrenExpressions())
  }

  def applyMethod(methodName: String, args: List[JavaExpressionOrQuery], context: JavaContext): JavaExpressionOrQuery = {
    (methodName, args) match {
      case ("filter", List(arg)) => this.filter(arg, context)
      case ("limitBy", List(orderingFunction, limitingNumberFunction)) =>
        this.limitBy(orderingFunction, limitingNumberFunction, context)
//      case ("head", Nil) =>
//        this.head(context)
      case ("reduce", List(start, map, reducer)) =>
        this.reduce(start, map, reducer, context)
      case ("sum", List(map)) =>
        this.sum(map, context)
      case ("count", List()) =>
        this.count(context)
      case (_, _) => JavaMethodCall(UnorderedQueryApplication(this), methodName, args)
    }
  }

  def filter(arg: JavaExpressionOrQuery, context: JavaContext): JavaExpressionOrQuery = {
    val thisClause = List(WhereClause.build(arg))

    this match {
      case UnorderedQuery(_, _, None, None) =>
        UnorderedQueryApplication(UnorderedQuery(source, whereClauses ++ thisClause, None, None))
      case _ => UnorderedQueryApplication(
        UnorderedQuery(UnorderedQueryApplication(this), thisClause, None, None))
    }
  }

  def limitBy(ordering: JavaExpressionOrQuery, limiting: JavaExpressionOrQuery, context: JavaContext): JavaExpressionOrQuery = {
    val thisClause = Some(LimitByClause.build(ordering, limiting, context))

    this match {
      case UnorderedQuery(_, _, None, None) =>
        UnorderedQueryApplication(
          UnorderedQuery(source, whereClauses, thisClause, None))
      case _ => UnorderedQueryApplication(
        UnorderedQuery(UnorderedQueryApplication(this), Nil, thisClause, None))
    }
  }

  def reduce(start: JavaExpressionOrQuery, map: JavaExpressionOrQuery, reducer: JavaExpressionOrQuery, context: JavaContext) = {
    this match {
      case UnorderedQuery(_, _, _, None) =>
        UnorderedQueryApplication(
          UnorderedQuery(source, whereClauses, limiter, Some(Reduction.build(start, map, reducer, context))))
    }
  }

  def sum(map: JavaExpressionOrQuery, context: JavaContext) = {
    val identityOnNumber = JavaLambdaExpr(List("x" -> JavaIntType), JavaVariable("x"))
    val sumOnNumber = JavaLambdaExpr(List("x" -> JavaIntType, "y" -> JavaIntType),
      JavaExpression.parse("x + y"))

    this match {
      case UnorderedQuery(_, _, _, None) =>
        UnorderedQueryApplication(
          UnorderedQuery(source, whereClauses, limiter, Some(
            Reduction.build(JavaMath(Number(0)), map, sumOnNumber, context))))
    }
  }

  def count(context: JavaContext) = {
    val constOneOnNumber = JavaLambdaExpr(List("x" -> JavaIntType), JavaMath(Number(1)))
    val sumOnNumber = JavaLambdaExpr(List("x" -> JavaIntType, "y" -> JavaIntType),
      JavaExpression.parse("x + y"))

    this match {
      case UnorderedQuery(_, _, _, None) =>
        UnorderedQueryApplication(
          UnorderedQuery(source, whereClauses, limiter, Some(
            Reduction.build(JavaMath(Number(0)), constOneOnNumber, sumOnNumber, context))))
    }
  }

//  def head(context: JavaContext) = this match {
//    case UnorderedQuery(_, _, None, None) =>
//      UnorderedQueryApplication(
//        UnorderedQuery(source, whereClauses, Some(LimitByClause.build(JavaLambdaExpr(List"x", JavaUnit), JavaMath(Number(1)), context)), None))
//  }
}


object UnorderedQuery {
  def blank(source: JavaExpressionOrQuery) = UnorderedQuery(source, Nil, None, None)
}

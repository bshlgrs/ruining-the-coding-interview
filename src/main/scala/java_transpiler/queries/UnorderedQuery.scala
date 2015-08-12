package java_transpiler.queries

import java_transpiler._

import ast_renderers.RubyOutputter
import cas.{Number, Name}

case class UnorderedQuery(
            source: JavaExpressionOrQuery,
            whereClauses: Set[WhereClause],
            mbLimiter: Option[LimitByClause],
            mbReduction: Option[Reduction]) {

  lazy val targetIsMagic: Boolean = source.isInstanceOf[JavaVariable]

  val parameters = whereClauses.flatMap(_.freeVariables)++ mbLimiter.map(_.freeVariables) ++ mbReduction.map(_.freeVariables)

  def childrenExpressions(): List[JavaExpressionOrQuery] = {
    whereClauses.flatMap(_.childrenExpressions()).toList
  }

  val allowedMethods: Map[String, Int] = Map("head" -> 0)

  def applyMethod(methodName: String, args: List[JavaExpressionOrQuery], context: JavaContext): JavaExpressionOrQuery = {
    (methodName, args) match {
      case ("filter", List(arg)) => this.filter(arg, context)
      case ("limitBy", List(orderingFunction, limitingNumberFunction)) =>
        this.limitBy(orderingFunction, limitingNumberFunction, context)
      case ("reduce", List(start, map, reducer)) =>
        this.reduce(start, map, reducer, context)
      case ("sum", List(map)) =>
        this.sum(map, context)
      case ("count", List()) =>
        this.count(context)
      case (_, _) =>
        if (allowedMethods.contains(methodName))
          if (allowedMethods(methodName) == args.length)
            JavaMethodCall(UnorderedQueryApplication(this),methodName, args)
          else
            throw new RuntimeException(s"call $methodName to $source has wrong number of args: ${args.length} instead of ${allowedMethods(methodName)}")
        else
          throw new RuntimeException(s"there is no method called $methodName on $source")
    }
  }

  def filter(arg: JavaExpressionOrQuery, context: JavaContext): JavaExpressionOrQuery = {
    val thisClause = Set(WhereClause.build(arg))

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
        UnorderedQuery(UnorderedQueryApplication(this), Set(), thisClause, None))
    }
  }

  def reduce(start: JavaExpressionOrQuery, map: JavaExpressionOrQuery, reducer: JavaExpressionOrQuery, context: JavaContext) = {
    this match {
      case UnorderedQuery(_, _, _, None) =>
        UnorderedQueryApplication(
          UnorderedQuery(source, whereClauses, mbLimiter, Some(Reduction.build(start, map, reducer, context))))
    }
  }

  def sum(map: JavaExpressionOrQuery, context: JavaContext) = {
    val identityOnNumber = JavaLambdaExpr(List("x" -> JavaIntType), JavaVariable("x"))
    val sumOnNumber = JavaLambdaExpr(List("x" -> JavaIntType, "y" -> JavaIntType),
      JavaExpression.parse("x + y"))

    this match {
      case UnorderedQuery(_, _, _, None) =>
        UnorderedQueryApplication(
          UnorderedQuery(source, whereClauses, mbLimiter, Some(
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
          UnorderedQuery(source, whereClauses, mbLimiter, Some(
            Reduction.build(JavaMath(Number(0)), constOneOnNumber, sumOnNumber, context))))
    }
  }

//  def head(context: JavaContext) = this match {
//    case UnorderedQuery(_, _, None, None) =>
//      UnorderedQueryApplication(
//        UnorderedQuery(source, whereClauses, Some(LimitByClause.build(JavaLambdaExpr(List"x", JavaUnit), JavaMath(Number(1)), context)), None))
//  }

  def toTrivialJavaExpression: JavaExpressionOrQuery = {
    val afterWhere = whereClauses.foldLeft(source) { (x: JavaExpressionOrQuery, y: WhereClause) =>
      JavaMethodCall(x, "select", List(y.toJavaLambdaExpression))
    }

    val afterLimit = mbLimiter match {
      case Some(limiter) => {
        JavaMethodCall(
          JavaMethodCall(afterWhere, "sort_by", List(limiter.orderingFunction)),
          "take",
          List(limiter.orderingFunction))
      }
      case None => afterWhere
    }

    mbReduction match {
      case Some(reduction) => {
        JavaMethodCall(
          JavaMethodCall(afterLimit, "map", List(reduction.mapper.asJavaLambda)),
          "inject",
          List(reduction.start, reduction.reducer.asJavaLambda)
        )
      }
      case None => afterLimit
    }
  }

  lazy val trickyWhereClauses = whereClauses.filter((x) => ! x.isConstant) // && ! x.isEqualitySeparable )
}

object UnorderedQuery {
  def blank(source: JavaExpressionOrQuery) = UnorderedQuery(source, Set(), None, None)
}

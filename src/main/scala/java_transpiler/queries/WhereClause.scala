package java_transpiler.queries

import java_transpiler._
import cas._

case class WhereClause(
                  nodeVariableName: String,
                  lhs: JavaExpressionOrQuery,
                  rhs: JavaExpressionOrQuery,
                  isEqualsInsteadOfGreaterThan: Boolean) {

  val constantWhereClause = ConstantWhereClause.build(this)
  val isConstant = constantWhereClause.isDefined

  val separableInequalityWhereClause = SeparableInequalityWhereClause.build(this)
  val separableEqualityWhereClause = SeparableEqualityWhereClause.build(this)

  val isSeparable = separableInequalityWhereClause.isDefined || separableEqualityWhereClause.isDefined
  val isEqualitySeparable = separableEqualityWhereClause.isDefined

  // just checking I haven't screwed up...
  assert(isSeparable || ! isConstant, s"a where clause ($this) is allegedly not separable but constant")

  def childrenExpressions(): List[JavaExpressionOrQuery] = {
    List(lhs, rhs)
  }

  lazy val freeVariables = childrenExpressions().flatMap(_.freeVariables).toSet - nodeVariableName

  def modify(astModifier: AstModifier): WhereClause = {
    WhereClause(nodeVariableName, lhs.modify(astModifier), rhs.modify(astModifier), isEqualsInsteadOfGreaterThan)
  }

  def replaceTarget(newTarget: String): WhereClause = {
    val map = Map(nodeVariableName -> JavaVariable(newTarget))
    WhereClause(newTarget, lhs.replaceVariables(map), rhs.replaceVariables(map), isEqualsInsteadOfGreaterThan)
  }

  def toJavaLambdaExpression = JavaLambdaExpr(List(nodeVariableName -> JavaIntType), toJavaExpression)

  lazy val toJavaExpression: JavaExpression = {
    val op = if (isEqualsInsteadOfGreaterThan)
      niceFunctions.equals.equals[JavaExpressionOrQuery]
    else
      niceFunctions.greaterThan.greaterThan[JavaExpressionOrQuery]

    JavaMath(CasFunctionApplication[JavaExpressionOrQuery](op, List(JavaMathHelper.casify(lhs), JavaMathHelper.casify(rhs))))
  }
}

object WhereClause {
  def build(predicate: JavaExpressionOrQuery): WhereClause = {
    predicate match {
      case JavaLambdaExpr(args, body) =>
        assert(args.length == 1, s"where clause $predicate")
        body match {
          case JavaMath(CasFunctionApplication(op, List(lhs, rhs))) =>
            if (op == niceFunctions.equals.equals)
              WhereClause(args.head._1,
                JavaMathHelper.decasify(lhs),
                JavaMathHelper.decasify(rhs),
                true)
            else if (op == niceFunctions.greaterThan.greaterThan)
              WhereClause(args.head._1,
                JavaMathHelper.decasify(lhs),
                JavaMathHelper.decasify(rhs),
                false)
            else
              throw new InternalTypeError(s"I have no idea how to do that: $predicate")
        }
      case _ => throw new InternalTypeError(s"where clause got the condition $predicate, which isn't a lambda")
}}}

abstract class WhereClauseNiceness


class ConstantWhereClause extends WhereClauseNiceness

object ConstantWhereClause {
  def build(whereClause: WhereClause): Option[ConstantWhereClause] = {
    if (whereClause.freeVariables.subsetOf(Set(whereClause.nodeVariableName)))
      Some(new ConstantWhereClause)
    else
      None
  }
}

case object SeparableInequalityWhereClause {
  def build(whereClause: WhereClause): Option[SeparableInequalityWhereClause] = {
    if (!whereClause.isEqualsInsteadOfGreaterThan)
      if (whereClause.lhs.freeVariables == Set())
        Some(SeparableInequalityWhereClause(whereClause.lhs, whereClause.rhs))
      else if (whereClause.rhs.freeVariables == Set())
        Some(SeparableInequalityWhereClause(whereClause.rhs, whereClause.lhs))
      else
        None
    else
      None
  }
}

case class SeparableInequalityWhereClause(
             nodeFunction: JavaExpressionOrQuery,
             paramFunction: JavaExpressionOrQuery) extends WhereClauseNiceness


case object SeparableEqualityWhereClause {
  def build(whereClause: WhereClause): Option[SeparableEqualityWhereClause] = {
    if (whereClause.isEqualsInsteadOfGreaterThan)
      if (whereClause.lhs.freeVariables == Set())
        Some(SeparableEqualityWhereClause(whereClause.lhs, whereClause.rhs))
      else if (whereClause.rhs.freeVariables == Set())
        Some(SeparableEqualityWhereClause(whereClause.rhs, whereClause.lhs))
      else
        None
    else
      None
  }
}

case class SeparableEqualityWhereClause(
               nodeFunction: JavaExpressionOrQuery,
               paramFunction: JavaExpressionOrQuery) extends WhereClauseNiceness


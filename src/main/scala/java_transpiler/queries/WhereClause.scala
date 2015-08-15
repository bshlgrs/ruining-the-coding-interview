package java_transpiler.queries

import java_transpiler._
import cas._

import scala.collection.generic.SeqFactory

case class WhereClause(
                  nodeVariableName: String,
                  body: JavaExpressionOrQuery) {

  val constantWhereClause = ConstantWhereClause.build(this)
  val isConstant = constantWhereClause.isDefined

  val separableInequalityWhereClause = SeparableInequalityWhereClause.build(this)
  val separableEqualityWhereClause = SeparableEqualityWhereClause.build(this)

  val isSeparable = separableInequalityWhereClause.isDefined || separableEqualityWhereClause.isDefined
  val isEqualitySeparable = separableEqualityWhereClause.isDefined

  // just checking I haven't screwed up...
  assert(isSeparable || ! isConstant, s"a where clause ($this) is allegedly not separable but constant")

  def childrenExpressions(): List[JavaExpressionOrQuery] = {
    List(body)
  }

  lazy val freeVariables = body.freeVariables - nodeVariableName

  def modify(astModifier: AstModifier): WhereClause = {
    WhereClause(nodeVariableName, body.modify(astModifier))
  }

  def replaceTarget(newTarget: String): WhereClause = {
    val map = Map(nodeVariableName -> JavaVariable(newTarget))
    WhereClause(newTarget, body.replaceVariables(map))
  }

  def toJavaLambdaExpression = JavaLambdaExpr(List(nodeVariableName -> JavaIntType), body)
}

object WhereClause {
  def build(predicate: JavaExpressionOrQuery): List[WhereClause] = {
    predicate match {
      case JavaLambdaExpr(args, body) =>
        assert(args.length == 1, s"where clause $predicate")
        buildBody(body).map(WhereClause(args.head._1, _))
      case _ => throw new InternalTypeError(s"where clause got the condition $predicate, which isn't a lambda")
    }
  }

  def buildBody(body: JavaExpressionOrQuery): List[JavaExpressionOrQuery] = body match {
    case JavaMath(x : BinaryOperatorApplication[JavaExpressionOrQuery]) if x.operator == logicalAnd.operator() =>
      buildBody(JavaMathHelper.decasify(x.lhs)) ++ buildBody(JavaMathHelper.decasify(x.rhs))
    case x => List(x)
  }
}

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
    whereClause.body match {
      case JavaMath(CasFunctionApplication(op, List(lhs, rhs))) if op.toString == ">" =>
        if (JavaMath(lhs).freeVariables - whereClause.nodeVariableName == Set())
          Some(SeparableInequalityWhereClause(JavaMathHelper.decasify(lhs), JavaMathHelper.decasify(rhs)))
        else if (JavaMath(rhs).freeVariables - whereClause.nodeVariableName == Set())
          Some(SeparableInequalityWhereClause(JavaMathHelper.decasify(rhs), JavaMathHelper.decasify(lhs)))
        else
          None
      case JavaMath(CasFunctionApplication(op, List(lhs, rhs))) if op.toString == "<" =>
        build(
          WhereClause(
            whereClause.nodeVariableName,
            JavaMath(CasFunctionApplication(JavaMathHelper.javaGreaterThan, List(rhs, lhs)))))
      case _ => None
    }
  }
}

case class SeparableInequalityWhereClause(
             nodeFunction: JavaExpressionOrQuery,
             paramFunction: JavaExpressionOrQuery) extends WhereClauseNiceness

case object SeparableEqualityWhereClause {
  def build(whereClause: WhereClause): Option[SeparableEqualityWhereClause] = whereClause.body match {
    case JavaMath(CasFunctionApplication(op, List(lhs, rhs))) if op.toString == "==" =>
      if (JavaMath(lhs).freeVariables - whereClause.nodeVariableName == Set())
        Some(SeparableEqualityWhereClause(JavaMathHelper.decasify(lhs), JavaMathHelper.decasify(rhs)))
      else if (JavaMath(rhs).freeVariables - whereClause.nodeVariableName == Set())
        Some(SeparableEqualityWhereClause(JavaMathHelper.decasify(rhs), JavaMathHelper.decasify(lhs)))
      else
        None
    case _ => None
  }
}

case class SeparableEqualityWhereClause(
               nodeFunction: JavaExpressionOrQuery,
               paramFunction: JavaExpressionOrQuery) extends WhereClauseNiceness


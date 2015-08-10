package java_transpiler.queries

import java_transpiler._
import cas._

case class WhereClause(
                  nodeVariableName: String,
                  lhs: JavaExpressionOrQuery,
                  rhs: JavaExpressionOrQuery,
                  isEqualsInsteadOfGreaterThan: Boolean,
                  context: JavaContext) {

  val constantWhereClause = ConstantWhereClause.build(this)
  val separableWhereClause = SeparableWhereClause.build(this)

  def childrenExpressions(): List[JavaExpressionOrQuery] = {
    List(lhs, rhs)
  }

  lazy val freeVariables = childrenExpressions().flatMap(_.freeVariables).toSet - nodeVariableName
}

object WhereClause {
  def build(predicate: JavaExpressionOrQuery, context: JavaContext): WhereClause = {
    predicate match {
      case JavaLambdaExpr(args, body) =>
        assert(args.length == 1, s"where clause $predicate")
        body match {
          case JavaMath(CasFunctionApplication(op, List(lhs, rhs))) =>
            if (op == niceFunctions.equals.equals)
              WhereClause(args.head._1,
                JavaBinaryOperation.decasify(lhs),
                JavaBinaryOperation.decasify(rhs),
                true,
                context)
            else if (op == niceFunctions.greaterThan.greaterThan)
              WhereClause(args.head._1,
                JavaBinaryOperation.decasify(lhs),
                JavaBinaryOperation.decasify(rhs),
                false,
                context)
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

case object SeparableWhereClause {
  def build(whereClause: WhereClause): Option[SeparableWhereClause] = {
    if (whereClause.lhs.freeVariables == Set())
      Some(SeparableWhereClause(whereClause.lhs, whereClause.rhs))
    else if (whereClause.rhs.freeVariables == Set())
      Some(SeparableWhereClause(whereClause.rhs, whereClause.lhs))
    else
      None
  }
}

case class SeparableWhereClause(
             nodeFunction: JavaExpressionOrQuery,
             paramFunction: JavaExpressionOrQuery) extends ConstantWhereClause


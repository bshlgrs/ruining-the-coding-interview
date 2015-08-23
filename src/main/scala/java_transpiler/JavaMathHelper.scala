package java_transpiler

import cas._
import com.github.javaparser.ast.expr.BinaryExpr

object JavaMathHelper {
  def opToMath[A](op: BinaryExpr.Operator, lhs: JavaExpression[A], rhs: JavaExpression[A]): JavaExpression[A] = {
    op match {
      case BinaryExpr.Operator.plus => JavaMath(casify(lhs) + casify(rhs))
      case BinaryExpr.Operator.times => JavaMath(casify(lhs) * casify(rhs))
      case BinaryExpr.Operator.minus => JavaMath(casify(lhs) - casify(rhs))
      case BinaryExpr.Operator.divide => JavaMath(casify(lhs) / casify(rhs))
      case BinaryExpr.Operator.equals => JavaMath(niceFunctions.equals(casify(lhs), casify(rhs)))
      case BinaryExpr.Operator.greater => JavaMath(niceFunctions.greaterThan(casify(lhs), casify(rhs)))
      case BinaryExpr.Operator.less => JavaMath(niceFunctions.greaterThan(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.greaterEquals => JavaMath(niceFunctions.greaterThanOrEquals(casify(lhs), casify(rhs)))
      case BinaryExpr.Operator.lessEquals => JavaMath(niceFunctions.greaterThanOrEquals(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.and => JavaMath(logicalAnd(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.binAnd => JavaMath(bitwiseAnd(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.or => JavaMath(logicalOr(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.binOr => JavaMath(bitwiseAnd(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.xor => JavaMath(bitwiseXor(casify(rhs), casify(lhs)))
      case BinaryExpr.Operator.remainder => JavaMath(modulo(casify(rhs), casify(lhs)))
    }
  }

  def casify[A](thing: JavaExpression[A]): MathExp[JavaExpression[A]] = thing match {
    case JavaMath(ast) => ast
    case _ => CasVariable(thing)
  }

  def decasify[A](thing: MathExp[JavaExpression[A]]): JavaExpression[A] = thing match {
    case CasVariable(exp) => exp
    case _ => JavaMath(thing)
  }

  def javaEquals[A] = niceFunctions.equals.equals[JavaExpression[A]]
  def javaGreaterThan[A] = niceFunctions.greaterThan.greaterThan[JavaExpression[A]]
}

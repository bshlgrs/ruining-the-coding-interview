package useful_data_structures

import java_transpiler._

import cas.{niceFunctions, Number}

object JavaAbbreviations {
  def jif(condition: JavaExpressionOrQuery,
          trueCase: List[JavaStatement],
          falseCase: List[JavaStatement] = Nil): JavaStatement = {
    IfStatement(condition, trueCase, falseCase)
  }

  def num(int: Int): JavaExpressionOrQuery = JavaMath(Number(int))

  def eq(lhs: JavaExpressionOrQuery, rhs: JavaExpressionOrQuery): JavaExpressionOrQuery =
    JavaMath(niceFunctions.equals(JavaMathHelper.casify(lhs), JavaMathHelper.casify(rhs)))

  def gt(lhs: JavaExpressionOrQuery, rhs: JavaExpressionOrQuery): JavaExpressionOrQuery =
    JavaMath(niceFunctions.greaterThan(JavaMathHelper.casify(lhs), JavaMathHelper.casify(rhs)))

  def lt(lhs: JavaExpressionOrQuery, rhs: JavaExpressionOrQuery): JavaExpressionOrQuery =
    JavaMath(niceFunctions.greaterThan(JavaMathHelper.casify(rhs), JavaMathHelper.casify(lhs)))

  def jv(string: String) = JavaVariable(string)
  //  def call(thing: JavaExpressionOrQuery, methodName: String, args: List[JavaExpressionOrQuery]) =
//    JavaMethodCall(thing, methodName, args)
}

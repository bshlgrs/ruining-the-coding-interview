package java_transpiler

import java_transpiler.queries.UnorderedQuery

import com.sun.javafx.fxml.expression.VariableExpression
import useful_data_structures.UsefulUnorderedDataStructure

abstract class AstModifier[A, B]() {
  def stmtMapper(statement: JavaStatement[A]): List[JavaStatement[B]]
  def exprMapper(expr: JavaExpression[A]): JavaExpression[B]

  def applyToStmt(stmt: JavaStatement[A]): List[JavaStatement[B]] = stmtMapper(stmt)
  def applyToExpr(expr: JavaExpression[A]): JavaExpression[B] = exprMapper(expr)

  private def apply(thing: Any): Any = thing match {
    case expr: JavaExpression[A] => applyToExpr(expr)
    case stmt: JavaStatement[A] => applyToStmt(stmt)
  }

  def mapOverStmt(stmt: JavaStatement[A]): JavaStatement[B] = stmt match {
    case VariableDeclarationStatement(name, javaType, initialValue) =>
      VariableDeclarationStatement(name, javaType, initialValue.map(applyToExpr))
    case ReturnStatement(value) => ReturnStatement(applyToExpr(value))
    case ExpressionStatement(exp) => ExpressionStatement(applyToExpr(exp))
    case IfStatement(cond, trueCase, falseCase) =>
      IfStatement(applyToExpr(cond), trueCase.flatMap(applyToStmt), falseCase.flatMap(applyToStmt))
    case WhileStatement(cond, action) =>
      WhileStatement(applyToExpr(cond), action.flatMap(applyToStmt))
  }

  def mapOverClass(javaClass: JavaClass): JavaClass = javaClass.modifyWithAstModifier(this)
}

case class VariableReplacer[A](map: Map[String, JavaExpression[A]]) extends AstModifier[A, A] {
  def stmtMapper(stmt: JavaStatement[A]) = List(stmt)
  def exprMapper(expr: JavaExpression[A]) = expr match {
    case JavaVariable(name) => map.getOrElse(name, JavaVariable(name))
    case _ => expr
  }
}

package java_transpiler

import java_transpiler.queries.UnorderedQuery

import com.sun.javafx.fxml.expression.VariableExpression
import useful_data_structures.UsefulUnorderedDataStructure

abstract class AstModifier[A, B]() {
  def stmtMapper(statement: JavaStatement[A]): List[JavaStatement[B]]
  def exprMapper(expr: JavaExpression[A]): JavaExpression[B]

  def applyToStmt(stmt: JavaStatement[A]): List[JavaStatement[B]] = stmtMapper(mapOverStmt(stmt))
  def applyToExpr(expr: JavaExpression[A]): JavaExpression[B] = exprMapper(mapOverExpr(expr))

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

  def mapOverExpr(expr: JavaExpression[A]): JavaExpression[B] = expr match {
    case JavaNull => JavaNull[B]
    case JavaBoolLit(bool) => JavaBoolLit(bool)
    case JavaMethodCall(callee, name, args) => JavaMethodCall(applyToExpr(callee), name, args.map(applyToExpr))
    case JavaFieldAccess(thing, field) => JavaFieldAccess(applyToExpr(thing), field)
    case JavaNewObject(className, typeArgs, args) => JavaNewObject(className, typeArgs, args.map(applyToExpr))
    case JavaThis => JavaThis[B]
    case JavaVariable(name) => JavaVariable(name)
    case JavaIfExpression(cond, ifTrue, ifFalse) => JavaIfExpression(applyToExpr(cond), applyToExpr(ifTrue), applyToExpr(ifFalse))
    case JavaLambdaExpr(args, body) => JavaLambdaExpr(args, applyToExpr(body))
    case JavaUnit => JavaUnit[B]
    case JavaAssignmentExpression(name, local, value) => JavaAssignmentExpression(name, local, applyToExpr(value))
    case JavaArrayInitializerExpr(items) => JavaArrayInitializerExpr(items.map(applyToExpr))
    case JavaStringLiteral(string) => JavaStringLiteral(string)
    case JavaMath(math) => JavaMath(math.mapOverVariables(applyToExpr))
    case x: PeculiarExpression[A] =>
//    case UnorderedQueryApplication(UnorderedQuery(source, wheres, limiter, reduction)) => {
//      val newQuery = UnorderedQuery(source, wheres.map(_.modify(this)), limiter.map(_.modify(this)), reduction.map(_.modify(this)))
//
//      UnorderedQueryApplication(newQuery)
//    }
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

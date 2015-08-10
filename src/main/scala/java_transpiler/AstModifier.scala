package java_transpiler

import com.sun.javafx.fxml.expression.VariableExpression

abstract class AstModifier() {
  def stmtMapper(statement: JavaStatement): List[JavaStatement]
  def exprMapper(expr: JavaExpressionOrQuery): JavaExpressionOrQuery

  def applyToStmt(stmt: JavaStatement): List[JavaStatement] = stmtMapper(mapOverStmt(stmt))
  def applyToExpr(expr: JavaExpressionOrQuery): JavaExpressionOrQuery = exprMapper(mapOverExpr(expr))

  private def apply(thing: Any): Any = thing match {
    case expr: JavaExpressionOrQuery => applyToExpr(expr)
    case stmt: JavaStatement => applyToStmt(stmt)
  }

  def mapOverStmt(stmt: JavaStatement): JavaStatement = stmt match {
    case VariableDeclarationStatement(name, javaType, initialValue) =>
      VariableDeclarationStatement(name, javaType, initialValue.map(applyToExpr))
    case ReturnStatement(value) => ReturnStatement(applyToExpr(value))
    case ExpressionStatement(exp) => ExpressionStatement(applyToExpr(exp))
    case IfStatement(cond, trueCase, falseCase) =>
      IfStatement(applyToExpr(cond), trueCase.flatMap(applyToStmt), falseCase.flatMap(applyToStmt))
    case WhileStatement(cond, action) =>
      WhileStatement(applyToExpr(cond), action.flatMap(applyToStmt))
  }

  def mapOverExpr(expr: JavaExpressionOrQuery): JavaExpressionOrQuery = expr match {
    case JavaNull => JavaNull
    case expr: JavaBoolLit => expr
    case JavaMethodCall(callee, name, args) => JavaMethodCall(applyToExpr(callee), name, args.map(applyToExpr))
    case JavaFieldAccess(thing, field) => JavaFieldAccess(applyToExpr(thing), field)
    case JavaNewObject(className, typeArgs, args) => JavaNewObject(className, typeArgs, args.map(applyToExpr))
    case JavaThis => JavaThis
    case expr: JavaVariable => expr
    case JavaIfExpression(cond, ifTrue, ifFalse) => JavaIfExpression(applyToExpr(cond), applyToExpr(ifTrue), applyToExpr(ifFalse))
    case JavaLambdaExpr(args, body) => JavaLambdaExpr(args, applyToExpr(body))
    case JavaUnit => JavaThis
    case JavaAssignmentExpression(name, local, value) => JavaAssignmentExpression(name, local, applyToExpr(value))
    case JavaArrayInitializerExpr(items) => JavaArrayInitializerExpr(items.map(mapOverExpr))
    case expr: JavaStringLiteral => expr
    case expr: JavaMath => JavaMath(expr.math.mapOverVariables(mapOverExpr))
  }

  def mapOverClass(javaClass: JavaClass): JavaClass = javaClass.modifyWithAstModifier(this)
}

case class VariableReplacer(map: Map[String, JavaExpressionOrQuery]) extends AstModifier {
  def stmtMapper(stmt: JavaStatement) = List(stmt)
  def exprMapper(expr: JavaExpressionOrQuery) = expr match {
    case JavaVariable(name) => map.getOrElse(name, JavaVariable(name))
    case _ => expr
  }
}

package java_transpiler

import java_transpiler.queries.JavaContext

import cas.Name
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import scala.collection.JavaConverters._
import scala.util.Try

sealed abstract class JavaStatement[A] {
  def childStatements: List[JavaStatement[A]] = this match {
    case _: VariableDeclarationStatement[A] => Nil
    case _: ReturnStatement => Nil
    case _: ExpressionStatement => Nil
    case IfStatement(cond, trueCase, falseCase) => trueCase ++ falseCase
    case WhileStatement(cond, action) => action
  }

  def childExpressions: List[JavaExpression[A]] = this match {
    case s: VariableDeclarationStatement[A] => s.initialValue.toList
    case s: ReturnStatement[A] => List(s.value)
    case s: ExpressionStatement[A] => List(s.value)
    case s: IfStatement[A] => List(s.cond)
    case s: WhileStatement[A] => List(s.cond)
  }

  def descendantExpressions: List[JavaExpression[A]] = {
    this.childExpressions.flatMap(_.descendantExpressions()) ++ descendantStatements.flatMap(_.childExpressions)
  }

  def descendantStatements: List[JavaStatement[A]] =
    List(this) ++ childStatements.flatMap(_.descendantStatements)

  def modify(astModifier: AstModifier): List[JavaStatement] = astModifier.applyToStmt(this)

//  def querify(c: JavaContext): JavaStatement = this match {
//    case stmt@VariableDeclarationStatement(_, _, initialValue) =>
//      stmt.copy(initialValue = initialValue.map(_.querify(c)))
//    case ReturnStatement(value) => ReturnStatement(value.querify(c))
//    case ExpressionStatement(value) => ExpressionStatement(value.querify(c))
//    case IfStatement(cond, trueCase, falseCase) =>
//      IfStatement(cond.querify(c), trueCase.map(_.querify(c)), falseCase.map(_.querify(c)))
//    case WhileStatement(cond, body) =>
//      WhileStatement(cond.querify(c), body.map(_.querify(c)))
//  }
}

case class VariableDeclarationStatement[A](name: String, javaType: JavaType, initialValue: Option[JavaExpression[A]]) extends JavaStatement[A]
case class ReturnStatement[A](value: JavaExpression[A]) extends JavaStatement[A]
case class ExpressionStatement[A](value: JavaExpression[A]) extends JavaStatement[A]
case class IfStatement[A](cond: JavaExpression[A], trueCase: List[JavaStatement[A]], falseCase: List[JavaStatement[A]]) extends JavaStatement[A]
case class WhileStatement[A](cond: JavaExpression[A], action: List[JavaStatement[A]]) extends JavaStatement[A]

object JavaStatement {
  def buildBlock(blk: BlockStmt): List[JavaStatement[Nothing]] = {
    Option(blk.getStmts).map(_.asScala.toList).getOrElse(Nil).map(buildStatement)
  }

  def buildPotentiallyBlock(stmt: Statement): List[JavaStatement[Nothing]] = stmt match {
    case blk: BlockStmt => buildBlock(blk)
    case _ => List(buildStatement(stmt))
  }

  def buildStatement(stmt: Statement): JavaStatement[Nothing] = stmt match {
    case s: ExpressionStmt =>
      s.getExpression match {
        case e: VariableDeclarationExpr =>
          val name = e.getVars.get(0).getId.getName
          val javaType = JavaType.build(e.getType)

          val body = Try(e.getChildrenNodes.get(1).asInstanceOf[VariableDeclarator].getInit).toOption.flatMap(Option(_))
          VariableDeclarationStatement(name, javaType, body.map(JavaExpression.build))
        case e: Expression => ExpressionStatement(JavaExpression.build(e))
      }
    case s: IfStmt =>
      val cond = JavaExpression.build(s.getCondition)
      val thenCase = JavaStatement.buildPotentiallyBlock(s.getThenStmt)
      val elseCase = JavaStatement.buildPotentiallyBlock(s.getElseStmt)
      IfStatement[Nothing](cond, thenCase, elseCase)
    case s: ReturnStmt => ReturnStatement(JavaExpression.build(s.getExpr))
    case _ =>
      println(s"$stmt : ${stmt.getClass} not implemented, you should do it")
      ???
  }

  def parse(stuff: String): JavaStatement = JavaMethodDeclaration.parse(s"void f() { $stuff }").body.head
}

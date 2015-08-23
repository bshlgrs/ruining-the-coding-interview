package java_transpiler

import java_transpiler.queries._


import cas._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.ExpressionStmt
import scala.collection.JavaConverters._
import scala.util.Try

sealed abstract class JavaExpression[A]  {
  def descendantExpressions(): List[JavaExpression[A]] = {
    this +: childrenExpressions().flatMap(_.descendantExpressions())
  }

  def childrenExpressions(): List[JavaExpression[A]]

  // this is overwritten by lambda, obviously
  def freeVariables: Set[String] = {
    childrenExpressions().flatMap(_.freeVariables).toSet
  }

  def modify(astModifier: AstModifier): JavaExpression[A] = astModifier.applyToExpr(this)

  def replaceVariables(map: Map[String, JavaExpression[A]]): JavaExpression[A] = {
    this.modify(VariableReplacer(map))
  }

  def call(methodName: String, args: List[JavaExpression[A]] = Nil) = JavaMethodCall(this, methodName, args)


  def childrenExpressions(): List[JavaExpression[A]] = this match {
    case JavaNull => Nil
    case expr: JavaBoolLit => Nil
    case JavaMethodCall(callee, name, args) => List(callee) ++ args
    case JavaFieldAccess(thing, field) => List(thing)
    case JavaNewObject(className, typeArgs, args) => args
    case JavaThis => Nil
    case expr: JavaVariable => Nil
    case JavaIfExpression(cond, ifTrue, ifFalse) => List(cond, ifTrue, ifFalse)
    case JavaLambdaExpr(args, body) => List(body)
    case JavaUnit => Nil
    case JavaAssignmentExpression(name, local, value) => List(value)
    case JavaArrayInitializerExpr(items) => items
    case expr: JavaStringLiteral => Nil
    case JavaMath(math) => math.variables.toList
  }

//  def querify(c: JavaContext): JavaExpression[A] = this match {
//    case JavaNull => this
//    case expr: JavaBoolLit => this
//    case JavaMethodCall(callee, name, args) => querifyMethodCall(callee.querify(c), name, args.map(_.querify(c)), c)
//    case JavaFieldAccess(thing, field) => JavaFieldAccess(thing.querify(c), field)
//    case JavaNewObject(className, typeArgs, args) => JavaNewObject(className, typeArgs, args.map(_.querify(c)))
//    case JavaThis => this
//    case expr: JavaVariable => this
//    case JavaIfExpression(cond, ifTrue, ifFalse) => JavaIfExpression(cond.querify(c), ifTrue.querify(c), ifFalse.querify(c))
//    case JavaLambdaExpr(args, body) => JavaLambdaExpr(args, body.querify(c))
//    case JavaUnit => this
//    case JavaAssignmentExpression(name, local, value) => JavaAssignmentExpression(name, local, value.querify(c))
//    case JavaArrayInitializerExpr(items) => JavaArrayInitializerExpr(items.map(_.querify(c)))
//    case expr: JavaStringLiteral => this
//    case JavaMath(math) => JavaMathHelper.decasify(math.mapOverVariables(_.querify(c)))
//  }
}

case object JavaNull extends JavaExpression[_]
case class JavaBoolLit[A](boolean: Boolean) extends JavaExpression[A]
case class JavaMethodCall[A](callee: JavaExpression[A], methodName: String, args: List[JavaExpression[A]]) extends JavaExpression[A]
case class JavaFieldAccess[A](thing: JavaExpression[A], field: String) extends JavaExpression[A]
case class JavaNewObject[A](className: String, typeArgs: List[JavaType], args: List[JavaExpression[A]]) extends JavaExpression[A]
case object JavaThis extends JavaExpression[_]
case class JavaVariable[A](name: String) extends JavaExpression[A] {
  override def freeVariables: Set[String] = Set(name)
}
case class JavaIfExpression[A](cond: JavaExpression[A], ifTrue: JavaExpression[A], ifFalse: JavaExpression[A]) extends JavaExpression[A]
case class JavaLambdaExpr[A](args: List[(String, JavaType)], out: JavaExpression[A]) extends JavaExpression[A] {
  override def freeVariables: Set[String] = out.freeVariables -- args.map(_._1).toSet
}
case object JavaUnit extends JavaExpression[_]
case class JavaAssignmentExpression[A](name: String, local: Boolean, expression: JavaExpression[A]) extends JavaExpression[A]
case class JavaArrayInitializerExpr[A](items: List[JavaExpression[A]]) extends JavaExpression[A]
case class JavaStringLiteral[A](string: String) extends JavaExpression[A]
case class JavaMath[A](math: MathExp[JavaExpression[A]]) extends JavaExpression[A]
abstract class PeculiarExpression[A] extends JavaExpression[A]

object JavaExpression {
  def build(exp: Expression): JavaExpression[Nothing] = exp match {
    case null => ???
    case exp: IntegerLiteralExpr =>
      JavaMath(Number(exp.getValue.toInt))
    case exp: AssignExpr =>
      val (lhs, isLocal) = exp.getTarget match {
        case f: FieldAccessExpr => (f.getField, false)
        case n: NameExpr => (n.getName, true)
      }

      val mbOp = exp.getOperator match {
        case AssignExpr.Operator.assign => None
        case AssignExpr.Operator.plus => Some(BinaryExpr.Operator.plus)
        case AssignExpr.Operator.minus => Some(BinaryExpr.Operator.minus)
        case _ => ???
      }

      val outExp = mbOp match {
        case None => build(exp.getValue)
        case Some(op) => JavaMathHelper.opToMath(op, JavaFieldAccess[Nothing](JavaThis, lhs), build(exp.getValue))
      }
      JavaAssignmentExpression(lhs, isLocal, outExp)
    case exp: BinaryExpr =>
      JavaMathHelper.opToMath(exp.getOperator, build(exp.getLeft), build(exp.getRight)).asInstanceOf[JavaExpression]
    //    case exp: MethodCallExpr =>
    //      ???
    case exp: NameExpr => JavaVariable(exp.getName)
    case exp: FieldAccessExpr => JavaFieldAccess(build(exp.getScope), exp.getField)
    case exp: ThisExpr => JavaThis
    case exp: ObjectCreationExpr =>
      val javaArgs = Option(exp.getArgs).map(_.asScala.toList)
      val args = javaArgs.getOrElse(List())
      val name = exp.getType.getName
      val typeArgs = Option(exp.getTypeArgs).map(_.asScala.toList).getOrElse(Nil).map(JavaType.build)
      JavaNewObject(name, typeArgs, args.map(build))
    case exp: LambdaExpr => exp.getBody match {
      case stmt: ExpressionStmt =>
        val params = exp.getParameters.asScala.map(_.getId.getName -> JavaIntType).toList
        JavaLambdaExpr(params, build(stmt.getExpression))
      case _ =>
        throw new RuntimeException("I can't deal with non-expression contents of lambdas yet. Oh well, neither can Python.")
    }
    case exp: ArrayInitializerExpr =>
      JavaArrayInitializerExpr(Option(exp.getValues).map(_.asScala.map(build)).getOrElse(Nil).toList)
    case exp: StringLiteralExpr =>
      JavaStringLiteral(exp.getValue)
    case exp: BooleanLiteralExpr =>
      JavaBoolLit(exp.getValue)
    case exp: MethodCallExpr =>
      val args = Try(exp.getArgs.asScala.toList).getOrElse(Nil).map(build)
      val scope = Option(exp.getScope).map(build).getOrElse(JavaThis)
      JavaMethodCall(scope, exp.getName, args)
    case exp: VariableDeclarationExpr =>
      throw new RuntimeException("this case should be handled in the JavaStatement#build method :/")
    case exp: NullLiteralExpr =>
      JavaNull
    case exp: UnaryExpr if exp.getOperator == UnaryExpr.Operator.negative =>
      JavaMathHelper.opToMath(BinaryExpr.Operator.minus, JavaMath(Number(0)), build(exp.getExpr)).asInstanceOf[JavaExpression]
    case _ =>
      println(s"$exp : ${exp.getClass} not implemented, do it man")
      ???
  }

  def parse(stuff: String): JavaExpression[A] = {
    JavaStatement.parse(s"int x = $stuff;").asInstanceOf[VariableDeclarationStatement].initialValue.get
  }

}

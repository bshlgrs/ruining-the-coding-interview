package java_transpiler

import java_transpiler.queries._


import cas._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.ExpressionStmt
import scala.collection.JavaConverters._
import scala.util.Try

sealed abstract class JavaExpression extends JavaExpressionOrQuery {
  def childrenExpressions(): List[JavaExpressionOrQuery] = this match {
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

  def querify(c: JavaContext): JavaExpressionOrQuery = this match {
    case JavaNull => this
    case expr: JavaBoolLit => this
    case JavaMethodCall(callee, name, args) => querifyMethodCall(callee.querify(c), name, args.map(_.querify(c)), c)
    case JavaFieldAccess(thing, field) => JavaFieldAccess(thing.querify(c), field)
    case JavaNewObject(className, typeArgs, args) => JavaNewObject(className, typeArgs, args.map(_.querify(c)))
    case JavaThis => this
    case expr: JavaVariable => this
    case JavaIfExpression(cond, ifTrue, ifFalse) => JavaIfExpression(cond.querify(c), ifTrue.querify(c), ifFalse.querify(c))
    case JavaLambdaExpr(args, body) => JavaLambdaExpr(args, body.querify(c))
    case JavaUnit => this
    case JavaAssignmentExpression(name, local, value) => JavaAssignmentExpression(name, local, value.querify(c))
    case JavaArrayInitializerExpr(items) => JavaArrayInitializerExpr(items.map(_.querify(c)))
    case expr: JavaStringLiteral => this
    case JavaMath(math) => JavaBinaryOperation.decasify(math.mapOverVariables(_.querify(c)))
  }

  private def querifyMethodCall(callee: JavaExpressionOrQuery,
                                name: String,
                                args: List[JavaExpressionOrQuery],
                                context: JavaContext): JavaExpressionOrQuery = {
    name match {
      case "insert" => JavaMethodCall(callee, name, args)
      case "remove" => JavaMethodCall(callee, name, args)
      case _ => {
        val mbQuerifiedCallee: Option[UnorderedQuery] = callee match {
          case callee @ UnorderedQueryApplication(query) => Some(query)
          case JavaVariable(innerName) if context.unorderedMultisets.keys.toSet.contains(innerName) => {
            Some(UnorderedQuery.blank(callee))
          }
          case _ => None
        }

        mbQuerifiedCallee match {
          case None => JavaMethodCall(callee, name, args)
          case Some(querifiedCallee) => querifiedCallee.applyMethod(name, args, context)
        }
      }
    }
  }
}

case object JavaNull extends JavaExpression
case class JavaBoolLit(boolean: Boolean) extends JavaExpression
case class JavaMethodCall(callee: JavaExpressionOrQuery, methodName: String, args: List[JavaExpressionOrQuery]) extends JavaExpression
case class JavaFieldAccess(thing: JavaExpressionOrQuery, field: String) extends JavaExpression
case class JavaNewObject(className: String, typeArgs: List[JavaType], args: List[JavaExpressionOrQuery]) extends JavaExpression
case object JavaThis extends JavaExpression
case class JavaVariable(name: String) extends JavaExpression {
  override def freeVariables: Set[String] = Set(name)
}
case class JavaIfExpression(cond: JavaExpressionOrQuery, ifTrue: JavaExpressionOrQuery, ifFalse: JavaExpressionOrQuery) extends JavaExpression
case class JavaLambdaExpr(args: List[(String, JavaType)], out: JavaExpressionOrQuery) extends JavaExpression {
  override def freeVariables: Set[String] = out.freeVariables -- args.map(_._1).toSet
}
case object JavaUnit extends JavaExpression
case class JavaAssignmentExpression(name: String, local: Boolean, expression: JavaExpressionOrQuery) extends JavaExpression
case class JavaArrayInitializerExpr(items: List[JavaExpressionOrQuery]) extends JavaExpression
case class JavaStringLiteral(string: String) extends JavaExpression
case class JavaMath(math: MathExp[JavaExpressionOrQuery]) extends JavaExpression

object JavaExpression {
  def build(exp: Expression): JavaExpression = exp match {
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
        case Some(op) => JavaBinaryOperation.opToMath(op, JavaFieldAccess(JavaThis, lhs), build(exp.getValue))
      }
      JavaAssignmentExpression(lhs, isLocal, outExp)
    case exp: BinaryExpr =>
      JavaBinaryOperation.opToMath(exp.getOperator, build(exp.getLeft), build(exp.getRight)).asInstanceOf[JavaExpression]
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
    case _ =>
      println(s"$exp : ${exp.getClass} not implemented, do it man")
      ???
  }

  def parse(stuff: String): JavaExpressionOrQuery = {
    JavaStatement.parse(s"int x = $stuff;").asInstanceOf[VariableDeclarationStatement].initialValue.get
  }

}

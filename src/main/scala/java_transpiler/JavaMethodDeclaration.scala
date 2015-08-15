package java_transpiler

import java_parser.JavaParserWrapper

import cas._
import java_transpiler.queries._
import com.github.javaparser.ast.body._
import scala.collection.JavaConverters._



case class JavaMethodDeclaration(name: String,
                                 returnType: Option[JavaType],
                                 isStatic: Boolean,
                                 args: List[(String, JavaType)],
                                 body: List[JavaStatement]) {
  def modifyWithAstModifier(astModifier: AstModifier): JavaMethodDeclaration = {
    JavaMethodDeclaration(name, returnType, isStatic, args, body.flatMap(astModifier.applyToStmt))
  }

  def querify(c: JavaContext): JavaMethodDeclaration = {
    this.copy(body = body.map(_.querify(c)))
  }

  def queries(): Set[UnorderedQuery] = body.flatMap(_.descendantExpressions).collect({
    case UnorderedQueryApplication(q) => q
  }).toSet

  lazy val descendantExpressions: List[JavaExpressionOrQuery] = body.flatMap(_.descendantExpressions)
  lazy val descendantStatements: List[JavaStatement] = body.flatMap(_.descendantStatements)

  lazy val variables: List[VariableScopeDetails] = {
    val argVariables = args.map((tuple) => VariableScopeDetails(tuple._1, tuple._2, true))
    val localVariables = body.flatMap(_.descendantStatements).toSet.flatMap((x: JavaStatement) => x match {
      case VariableDeclarationStatement(varName, javaType, _) => List(VariableScopeDetails(varName, javaType, false))
      case _ => Nil
    })

    argVariables ++ localVariables
  }
}

case class JavaConstructorDeclaration(args: List[(String, JavaType)],
                                      body: List[JavaStatement]) {
  def modifyWithAstModifier(astModifier: AstModifier): JavaConstructorDeclaration = {
    JavaConstructorDeclaration(args, body.flatMap(astModifier.applyToStmt))
  }
}

object JavaMethodDeclaration {
  def build(methodDeclaration: MethodDeclaration) = {
    val name = methodDeclaration.getName

    if (name == "initialize")
      throw new RuntimeException("method name cannot be 'initialize' for Ruby compatibility reasons")

    val javaType = JavaType.buildOptionType(methodDeclaration.getType)

    // getModifiers returns some crazy bit flag bullshit, I should investigate further at some point.
    val isStatic = (methodDeclaration.getModifiers & 8) == 8

    val args = Option(methodDeclaration.getParameters).map(_.asScala.toList.map({ (x) =>
      (x.getId.getName, JavaType.build(x.getType))})).getOrElse(List())

    val body = JavaStatement.buildBlock(methodDeclaration.getBody)

    JavaMethodDeclaration(name, javaType, isStatic, args, body)
  }

  def maybeBuildConstructor(decl: ConstructorDeclaration): JavaConstructorDeclaration = {
    val args = Option(decl.getParameters).map(_.asScala.toList.map({ (x) =>
      (x.getId.getName, JavaType.build(x.getType))})).getOrElse(List())

    val body = JavaStatement.buildBlock(decl.getBlock)

    JavaConstructorDeclaration(args, body)
  }

  def parse(string: String): JavaMethodDeclaration = {
    JavaParserWrapper.parseJavaClassToAst(s"class Example { $string }").methods.head
  }

  def main(args: Array[String]) {
    val string = "int factorial() { Ant output[]; Bee<Cat> output; if (x==0) output = 1; else output = factorial(x-1) * x; return output; }"
    println(parse(string).variables.mkString("\n"))
  }
}

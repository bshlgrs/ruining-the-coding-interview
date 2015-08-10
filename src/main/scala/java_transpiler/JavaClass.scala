package java_transpiler

import java_parser.JavaParserWrapper
import java_transpiler.queries.{UnorderedQuery, JavaContext}

import com.github.javaparser.ast.body._
import scala.collection.JavaConverters._

case class JavaClass(name: String,
                     constructor: Option[JavaConstructorDeclaration],
                     fields: List[JavaFieldDeclaration],
                     methods: List[JavaMethodDeclaration],
                     innerClasses: List[JavaClass]) {
  def querify(): JavaClass = {
    val context = JavaContext(unorderedTables())
    this.copy(methods = methods.map(_.querify(context)))
  }

  def queries(): List[UnorderedQuery] = methods.flatMap(_.queries())

  def unorderedTables(): Map[String, JavaClass] = {
    magicMultisets().map((x) => x._1 -> getInnerClass(x._2).get)
  }

  def getMethod(name: String): Option[JavaMethodDeclaration] = {
    methods.find(_.name == name)
  }

  def getField(name: String): Option[JavaFieldDeclaration] = {
    fields.find(_.name == name)
  }

  def getInnerClass(name: String): Option[JavaClass] = innerClasses.find(_.name == name)

  def magicMultisets(): Map[String, String] = {
    fields.flatMap((fieldDeclaration: JavaFieldDeclaration) => fieldDeclaration.javaType match {
      case JavaClassType(classTypeName, args) if classTypeName == "MagicMultiset" =>
        List(fieldDeclaration.name -> args.head.toScalaTypeString())
      case _ => Nil
    }).toMap
  }

  def methodsCalledOnObject(name: String): List[String] = {
    methods.flatMap((method: JavaMethodDeclaration) =>
      method.body.flatMap(_.descendantExpressions).collect({
        case JavaMethodCall(JavaVariable(objectName), methodName, _) if objectName == name => methodName
      })
    )
  }

  def modifyWithAstModifier(astModifier: AstModifier): JavaClass = {
    JavaClass(
      name,
      constructor.map(_.modifyWithAstModifier(astModifier)),
      fields.map(_.modifyWithAstModifier(astModifier)),
      methods.map(_.modifyWithAstModifier(astModifier)),
      innerClasses.map(_.modifyWithAstModifier(astModifier))
    )
  }
}

object JavaClass {
  def build(typeDeclaration: TypeDeclaration): JavaClass = {
    val fieldDeclarations = typeDeclaration
      .getMembers
      .asScala
      .filter(_.isInstanceOf[FieldDeclaration])
      .map(_.asInstanceOf[FieldDeclaration])

    val fieldDeclarationAsts = fieldDeclarations.map(JavaFieldDeclaration.build).toList

    val methodDeclarations = typeDeclaration
      .getMembers
      .asScala
      .filter(_.isInstanceOf[MethodDeclaration])
      .map(_.asInstanceOf[MethodDeclaration])

    val methodDeclarationAsts = methodDeclarations.map(JavaMethodDeclaration.build).toList

    val constructorAst = typeDeclaration
      .getMembers
      .asScala
      .filter(_.isInstanceOf[ConstructorDeclaration])
      .map(_.asInstanceOf[ConstructorDeclaration])
      .headOption
      .map(JavaMethodDeclaration.maybeBuildConstructor)

    val innerClasses = typeDeclaration
      .getMembers
      .asScala
      .filter(_.isInstanceOf[ClassOrInterfaceDeclaration])
      .map(_.asInstanceOf[ClassOrInterfaceDeclaration])
      .map(build)
      .toList

    JavaClass(typeDeclaration.getName, constructorAst, fieldDeclarationAsts, methodDeclarationAsts, innerClasses)
  }

  def parse(string: String): JavaClass = {
    JavaParserWrapper.parseJavaClassToAst(s"class Example { $string }")
  }
}

package java_transpiler

import java_parser.JavaParserWrapper
import java_transpiler.queries.{UnorderedQuery, JavaContext}
import javax.management.Query

import com.github.javaparser.ast.body._
import useful_data_structures.UsefulUnorderedDataStructure
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

  val toType: JavaType = JavaClassType(this.name, Nil)

  def queries(): Set[UnorderedQuery] = methods.flatMap(_.queries()).toSet

  def actualizeQueries(auxiliaryDataStructures: Map[UnorderedQuery, Option[UsefulUnorderedDataStructure]]): JavaClass = {
    val actualizer = QueryActualizer(auxiliaryDataStructures, this)
    val classWithModifiedMethods = modifyWithAstModifier(actualizer)

    val queryMethods = auxiliaryDataStructures.values.flatten.toList.map(_.methodCode).flatten

    val modificationMethods = magicMultisets.flatMap((x) => x._2.modificationMethods(x._1, auxiliaryDataStructures))

    classWithModifiedMethods.copy(methods = classWithModifiedMethods.methods ++ queryMethods ++ modificationMethods)
  }

  def unorderedTables(): Map[String, JavaClass] = {
    magicMultisets.map(x => x._1 -> x._2.itemClass)
  }

  def getMethod(name: String): Option[JavaMethodDeclaration] = {
    methods.find(_.name == name)
  }

  def getField(name: String): Option[JavaFieldDeclaration] = {
    fields.find(_.name == name)
  }

  def getInnerClass(name: String): Option[JavaClass] = innerClasses.find(_.name == name)

  lazy val magicMultisets: Map[String, MagicMultiset] = {
    val names: Set[String] = fields.flatMap((fieldDeclaration: JavaFieldDeclaration) => fieldDeclaration.javaType match {
      case JavaClassType(classTypeName, args) if classTypeName == "MagicMultiset" =>
        Some(fieldDeclaration.name)
      case _ => None
    }).toSet

    names.map({(name) =>
      val fieldDeclaration = getField(name).getOrElse(throw new RuntimeException(s"oh god $name"))
      val javaClass = getInnerClass(fieldDeclaration.javaType.asInstanceOf[JavaClassType].itemTypes.head.toScalaTypeString())
        .getOrElse(throw new RuntimeException(s"oh god $fieldDeclaration"))
      val supportsInsert = methodsCalledOnObject(name).contains("insert")
      val supportsRemove = methodsCalledOnObject(name).contains("remove")
      name -> MagicMultiset(javaClass, supportsInsert, supportsRemove)}).toMap
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

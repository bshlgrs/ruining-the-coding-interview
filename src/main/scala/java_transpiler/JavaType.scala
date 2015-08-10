package java_transpiler

import java_parser.JavaParserWrapper

import com.github.javaparser.ast.`type`._

import scala.collection.JavaConverters._
import scala.util.Try

sealed abstract class JavaType {
  def toScalaTypeString(): String
}

case object JavaIntType extends JavaType {
  lazy val toScalaTypeString = "Int"
}

case object JavaBoolType extends JavaType {
  lazy val toScalaTypeString = "Boolean"
}

case class JavaArrayType(itemType: JavaType) extends JavaType {
  lazy val toScalaTypeString = s"Array[${itemType.toScalaTypeString()}]"
}

case class JavaClassType(name: String, itemTypes: List[JavaType]) extends JavaType {
  lazy val toScalaTypeString = itemTypes match {
    case Nil => name
    case _ => s"$name[${itemTypes.map(_.toScalaTypeString()).mkString(", ")}]"
  }
}

case class JavaFunctionType(argTypes: List[JavaType], returnType: Option[JavaType]) extends JavaType {
  lazy val toScalaTypeString = {
    val typeString = returnType.map(_.toScalaTypeString()).getOrElse("Unit")
    s"(${argTypes.mkString(", ")}) => $typeString"
  }
}

object JavaType {
  def build(thing: Type): JavaType = {
    thing match {
      case x: PrimitiveType =>
        x.getType match {
          case PrimitiveType.Primitive.Int => JavaIntType
          case PrimitiveType.Primitive.Boolean => JavaBoolType
          case _ =>
            ???
        }
      case x: ClassOrInterfaceType =>
        val typeArgs = Try(x.getTypeArgs.asScala.map(JavaType.build).toList).recover({
          case _: NullPointerException => Nil
        }).get
        JavaClassType(x.getName, typeArgs)
      case x: ReferenceType =>
        // previously JavaArrayType(_)
        build(x.getType)
      case _ =>
        ???
    }
  }

  def buildOptionType(thing: Type): Option[JavaType] = {
    thing match {
      case _: VoidType => None
      case _ => Some(build(thing))
    }

  }

  def parse(string: String): JavaType = {
    JavaParserWrapper.parseJavaClassToAst(s"class Example { $string x; }").fields.head.javaType
  }

  def main(args: Array[String]) {
    val parsedType = parse("Blah<Int>")
    println(parsedType)
    println(parsedType.toScalaTypeString())
  }
}

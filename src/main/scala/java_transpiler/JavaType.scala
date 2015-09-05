package java_transpiler

import java_parser.JavaParserWrapper

import com.github.javaparser.ast.`type`._

import scala.collection.JavaConverters._
import scala.util.Try

sealed abstract class JavaType[A] {
  def toScalaTypeString(): String
}

abstract class NormalJavaType[A] extends JavaType[A] {
  def mapOverChildren[B](astModifier: AstModifier[A, B]): JavaType[B] = this match {
    case JavaIntType => JavaIntType[B]
    case JavaBoolType => JavaBoolType[B]
    case JavaArrayType(itemType) => JavaArrayType(astModifier.modifyType(itemType))
    case JavaClassType(name, itemTypes) => JavaClassType(name, itemTypes.map(astModifier.modifyType))
  }

}

case object JavaIntType extends NormalJavaType[_] {
  lazy val toScalaTypeString = "Int"
}

case object JavaBoolType extends NormalJavaType[_] {
  lazy val toScalaTypeString = "Boolean"
}

case class JavaArrayType[A](itemType: JavaType[A]) extends NormalJavaType[A] {
  lazy val toScalaTypeString = s"Array[${itemType.toScalaTypeString()}]"
}

case class JavaClassType[A](name: String, itemTypes: List[JavaType[A]]) extends NormalJavaType[A] {
  lazy val toScalaTypeString = itemTypes match {
    case Nil => name
    case _ => s"$name[${itemTypes.map(_.toScalaTypeString()).mkString(", ")}]"
  }
}

case class JavaFunctionType[A](argTypes: List[JavaType[A]], returnType: Option[JavaType[A]]) extends NormalJavaType[A] {
  lazy val toScalaTypeString = {
    val typeString = returnType.map(_.toScalaTypeString()).getOrElse("Unit")
    s"(${argTypes.mkString(", ")}) => $typeString"
  }
}

abstract class PeculiarType[A] extends JavaType[A]

object JavaType {
  def build(thing: Type): JavaType[_] = {
    thing match {
      case x: PrimitiveType =>
        x.getType match {
          case PrimitiveType.Primitive.Int => JavaIntType[_]
          case PrimitiveType.Primitive.Boolean => JavaBoolType[_]
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

  def buildOptionType[A](thing: Type): Option[JavaType[_]] = {
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

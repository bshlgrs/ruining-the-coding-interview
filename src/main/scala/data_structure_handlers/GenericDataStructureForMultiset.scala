package data_structure_handlers

import java_parser.JavaParserWrapper
import java_transpiler.{JavaStatement, JavaClass, JavaMethodDeclaration}
import big_o.BigO
import com.github.javaparser.JavaParser

case class GenericDataStructureForMultiset(insertion: Option[MutatingMethodImplementation],
                           removal: Option[MutatingMethodImplementation],
                           time: JavaMethodDeclaration,
                           initialization: List[JavaStatement],
                           implementation: JavaMethodDeclaration) {

}

object GenericDataStructureForMultiset {
  def build(javaClass: JavaClass): GenericDataStructureForMultiset = {
    val insertion = javaClass.getField("timeForInsert").flatMap(_.initialValue).map { (javaTime) =>
      val time = BigO.fromJavaExpression(javaTime)
      val beforeInsert = javaClass.getMethod("beforeInsert").map(_.body).getOrElse(Nil)
      val afterInsert = javaClass.getMethod("afterInsert").map(_.body).getOrElse(Nil)

      MutatingMethodImplementation(time, beforeInsert, afterInsert)
    }

    val removal = javaClass.getField("timeForRemove").flatMap(_.initialValue).map { (javaTime) =>
      val time = BigO.fromJavaExpression(javaTime)
      val beforeInsert = javaClass.getMethod("beforeRemove").map(_.body).getOrElse(Nil)
      val afterInsert = javaClass.getMethod("afterRemove").map(_.body).getOrElse(Nil)

      MutatingMethodImplementation(time, beforeInsert, afterInsert)
    }

    val time = javaClass.getMethod("timeForQuery").getOrElse(throw new RuntimeException("needs timeForQuery"))

    val initialization = javaClass.getMethod("afterInitialize").map(_.body).getOrElse(Nil)

    val implementation = javaClass.getMethod("query").getOrElse(throw new RuntimeException("needs query"))

    new GenericDataStructureForMultiset(insertion, removal, time, initialization, implementation)
  }
}

package java_transpiler

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body._

import scala.collection.JavaConverters._

object AstBuilder {
  def build(cu: CompilationUnit): List[JavaClass] = {
    cu.getTypes.asScala.map(JavaClass.build).toList
  }
}

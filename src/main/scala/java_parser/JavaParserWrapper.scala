package java_parser

import java.io.StringBufferInputStream
import java_transpiler._

import ast_renderers.RubyOutputter
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.CompilationUnit
import useful_data_structures.UnorderedDataStructureLibrary

import scala.collection.mutable

object JavaParserWrapper {
  def main(args: Array[String]) {
    val node = parseJavaFile("")
    val classAsts = AstBuilder.build(node)
    println(classAsts)
    println(classAsts.map(RubyOutputter.outputClass).mkString("\n\n"))

    classAsts.foreach(RubyOutputter.outputClassToFile)
  }

  def parseJavaFile(filename: String): CompilationUnit = {
    parseJava(javaString)
  }

  def parseJava(java: String) = {
    // i know this is deprecated but IDGAF
    val stringBuffer = new StringBufferInputStream(java)
    JavaParser.parse(stringBuffer)
  }

  def parseJavaClassToAst(java: String): JavaClass = {
    AstBuilder.build(parseJava(java)).head
  }

  val javaString =
    """
       |class Counter {
       |  public Counter(int start) {
       |    this.x = start;
       |    new Counter(x -> x);
       |  }
       |
       |  void increase(int y) {
       |    this.x += y;
       |  }
       |  int get() {
       |    return this.x;
       |  }
       |}
    """.stripMargin('|')
}

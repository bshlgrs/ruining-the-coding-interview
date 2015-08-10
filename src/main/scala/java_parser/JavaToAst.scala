package java_parser

import java.io.StringBufferInputStream
import java_transpiler.AstBuilder

import com.github.javaparser.JavaParser
import com.github.javaparser.ast._

object JavaToAst {
  def main(args: Array[String]) {
    val node = parseJavaFile("")

    println(AstBuilder.build(node))
  }

  def parseJavaFile(filename: String): CompilationUnit = {
    // i know this is deprecated but IDGAF
    println(javaString)
    println("... turns into")
    val stringBuffer = new StringBufferInputStream(javaString)
    JavaParser.parse(stringBuffer)
  }

  val javaString =
    """
       class Counter {
         int x = 0;
         void increase(int y) {
           this.x += y;
         }
         int get() {
           return this.x;
         }
       }
    """
}

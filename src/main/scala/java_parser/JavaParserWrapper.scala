package java_parser

import java.io.StringBufferInputStream
import java_transpiler._

import ast_renderers.RubyOutputter
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.CompilationUnit

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

object ParserOfApi {
  def main(args: Array[String]) {
    val priorityQueue = JavaParserWrapper.parseJavaClassToAst(
      """
        |public class PriorityQueue  {
        |    class Item {
        |        int priority1;
        |        int priority2;
        |        int id;
        |    }
        |
        |    MagicMultiset<Item> stuff = new MagicMultiset<Item>();
        |
        |    int getCheapestByPriority1() {
        |        return stuff.limitBy(x -> x.priority1, 1).head();
        |    }
        |
        |    int getCheapestByPriority2() {
        |        return stuff.limitBy(x -> x.priority2, 1).head();
        |    }
        |
        |    int insertItem(int priority1, int priority2, int id) {
        |        stuff.insert(priority1, priority2, id);
        |    }
        |
        |    int popCheapest() {
        |        int cheapest = getCheapest();
        |        stuff.remove(cheapest);
        |        return cheapest;
        |    }
        |}
      """.stripMargin)

    val querified = priorityQueue.querify()

    println(querified.methods.head.descendantExpressions.mkString("\n___\n"))

    println(querified.queries())
    println(priorityQueue.queries())
  }

}

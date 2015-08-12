package useful_data_structures

import java_parser.JavaParserWrapper
import java_transpiler._
import ast_renderers.RubyOutputter

import scala.io.Source

object Optimizer {
  def optimize(jc: JavaClass): JavaClass = {
    val querified = jc.querify()

    val auxiliaryDataStructures = querified.queries().map({ (x) =>
      x -> UnorderedDataStructureLibrary.getBestStructureForClass(x, querified)
    }).toMap

    querified.actualizeQueries(auxiliaryDataStructures)
  }

  def main(args: Array[String]) {
    val javaSource = Source.fromFile(args.headOption.getOrElse("example-apis/example.java")).getLines().mkString("\n")

    val javaClass = JavaParserWrapper.parseJavaClassToAst(javaSource)

    val optimizedClass = optimize(javaClass)

    val rubyCode = RubyOutputter.outputClass(optimizedClass)

    println(rubyCode)
  }
}

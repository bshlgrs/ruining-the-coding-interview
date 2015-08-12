import java_parser.JavaParserWrapper
import java_transpiler.JavaClass

import ast_renderers.RubyOutputter
import useful_data_structures.UnorderedDataStructureLibrary

import scala.io.Source

object Optimizer {
  def optimize(jc: JavaClass): JavaClass = {
    val querified = jc.querify()

    val auxiliaryDataStructures = querified.queries().map({ (x) =>
      x -> UnorderedDataStructureLibrary.getBestStructureForClass(x, querified)
    }).toMap

    println(auxiliaryDataStructures)

    querified.actualizeQueries(auxiliaryDataStructures)
  }

  def unoptimized(jc: JavaClass): JavaClass = {
    val querified = jc.querify()

    querified.actualizeQueries(Map())
  }

  def main(args: Array[String]) {
    val javaSource = Source.fromFile(args.headOption.getOrElse("example-apis/example.java")).getLines().mkString("\n")

    val javaClass = JavaParserWrapper.parseJavaClassToAst(javaSource)

    val optimizedClass = optimize(javaClass)

    val rubyCode = RubyOutputter.outputClass(optimizedClass)

    println(rubyCode)
  }
}

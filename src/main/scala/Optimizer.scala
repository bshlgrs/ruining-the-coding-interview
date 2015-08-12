import java_parser.JavaParserWrapper
import java_transpiler.JavaClass

import ast_renderers.RubyOutputter
import useful_data_structures.UnorderedDataStructureLibrary

import scala.io.Source

object Optimizer {
  def optimize(jc: JavaClass): JavaClass = {
    val querified = jc.querify()

    println(RubyOutputter.outputClass(querified))

    val auxiliaryDataStructures = querified.queries().map({ (x) =>
      x -> UnorderedDataStructureLibrary.getBestStructureForClass(x, querified)
    }).toMap

    querified.actualizeQueries(auxiliaryDataStructures)
  }

  def main(args: Array[String]) {
    val javaSource = Source.fromFile(args.headOption.getOrElse("example-apis/PriorityQueue.java")).getLines().mkString("\n")

    val javaClass = JavaParserWrapper.parseJavaClassToAst(javaSource)

    val optimizedClass = optimize(javaClass)

    val rubyCode = RubyOutputter.outputClass(optimizedClass)

    println(rubyCode)
  }
}

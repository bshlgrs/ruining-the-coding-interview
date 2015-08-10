package ruby_to_ast

import java.io.StringReader

import org.jruby.CompatVersion
import org.jruby.ast.Node
import org.jruby.Ruby

import org.jruby.parser.{ParserConfiguration, Parser}

object RubyParserTester {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("example-apis/AverageAgeMultiset.rb")
    val lines = try source.mkString finally source.close()

    val node = parseContents(lines)

    println(node)
  }

  def parseContents(string: String): Node = {
    val rubyParser = new Parser(Ruby.newInstance())
    val in = string.toCharArray.map(_.toByte)
    val version = CompatVersion.BOTH
    val config = new ParserConfiguration(Ruby.newInstance(), 0, false, version)

    // sorry James for the null. I didn't make the api, I just use it...
    rubyParser.parse("fuck.rb", in, null, config)
  }
//  def parseContents(string: String) = {
//    ???
//  }
}


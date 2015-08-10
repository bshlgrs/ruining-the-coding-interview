package external_interfaces

import java.io._
import scala.sys.process._
import scala.io.Source

object ExternalInterfaces {
  def rubocopify(string: String) = {
    val writer = new PrintWriter(new File("/tmp/test.rb" ))
    writer.write(string)
    writer.close()

    "rubocop -a /tmp/test.rb" ! ProcessLogger(line => ())

    Source.fromFile("/tmp/test.rb").mkString("")
  }
}

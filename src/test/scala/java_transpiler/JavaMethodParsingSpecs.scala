package cas

import java_transpiler.{JavaIntType, JavaType, JavaMethodDeclaration}

import org.scalatest.{Matchers, FlatSpec, PropSpec}
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.{ShouldMatchers, MustMatchers}

class JavaMethodParsingSpecs extends FlatSpec with Matchers {
  "A method" should "be parsed to a correct AST" in {
    val java =
      """
        int factorial(int x) {
          if (x <= 0) return 1;
          else return factorial(x - 1) * x;
        }
      """

    val method = JavaMethodDeclaration.parse(java)

    method.args should be(List("x" -> JavaIntType))
    method.name should be("factorial")
    method.returnType should be(Some(JavaIntType))
  }
}

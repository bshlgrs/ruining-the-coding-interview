package cas

import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import org.scalatest.matchers.MustMatchers
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object ExpressionGenerators {
  lazy val genExpression: Gen[MathExp[Name]] = for {
    result <- Gen.oneOf(genVariable, genConstant, genSum, genProduct)
  } yield result

  lazy val genVariable: Gen[CasVariable[Name]] = {
    Gen.oneOf("x", "y", "z").map((name: String) => CasVariable(Name(name)))
  }

  lazy val genSum: Gen[MathExp[Name]] = for {
    x <- genVariable
    y <- genExpression
  } yield x + y

  lazy val genProduct: Gen[MathExp[Name]] = for {
    x <- genSum
    y <- Gen.oneOf(genProduct, genVariable)
    z <- Gen.listOf(genVariable)
  } yield (List(x, y) ++ z).reduce(_ * _)

  lazy val genConstant: Gen[MathExp[Name]] = Gen.oneOf(Number(0), Number(1), Number(2)).map(_.asInstanceOf[MathExp[Name]])

  implicit lazy val arbExpression: Arbitrary[MathExp[Name]] = Arbitrary(genExpression)
}

class ExpressionTests extends PropSpec with PropertyChecks with MustMatchers {
  import ExpressionGenerators._

  implicit lazy val arbInteger: Arbitrary[Int] = Arbitrary(Gen.chooseNum(-10, 10))

  type Exp = MathExp[Name]

  property("Expression generator isn't buggy") {
    forAll { (exp: Exp) =>
      exp must be(exp)
    }
  }

  property("Addition works on numbers") {
    forAll { (lhs: Int, rhs: Int) =>
      Number(lhs) + Number(rhs) must be (Number(lhs + rhs))
    }
  }

  property("Addition isn't buggy") {
    forAll { (lhs:Exp, rhs: Exp) =>
      (lhs + rhs) must be(lhs + rhs)
    }
  }

  property("Addition is commutative") {
    forAll { (lhs:Exp, rhs: Exp) =>
      (lhs + rhs) must be(rhs + lhs)
    }
  }

  property("Addition is commutative according to monte carlo") {
    forAll { (lhs:Exp, rhs: Exp) =>
      (lhs + rhs).monteCarloEquals(rhs + lhs) must be(true)
    }
  }

  property("Addition is associative") {
    forAll { (a: Exp, b: Exp, c: Exp) =>
      ((a + b) + c).simplify must be(a + (b + c))
    }
  }

  property("Addition is associative according to monte carlo") {
    forAll { (a:Exp, b: Exp, c: Exp) =>
      ((a + b) + c).monteCarloEquals(a + (b + c)) must be(true)
    }
  }

  property("Simplification only needs to be done once") {
    forAll { (exp:Exp) =>
      exp.simplify must be(exp.simplify.simplify)
    }
  }

  property("Multiplication works on numbers") {
    forAll { (lhs: Int, rhs: Int) =>
      Number(lhs) * Number(rhs) must be (Number(lhs * rhs))
    }
  }

  property("Multiplication works on expressions") {
    forAll { (lhs: Exp, rhs: Exp) =>
      lhs * rhs must be(lhs * rhs)
    }
  }

  property("Multiplication is commutative") {
    forAll { (lhs:Exp, rhs: Exp) =>
      (lhs * rhs) must be(rhs * lhs)
    }
  }

  property("Multiplication is associative") {
    forAll { (a: Exp, b: Exp, c: Exp) =>
      ((a * b) * c).simplify must be(a * (b * c))
    }
  }

  property("Min is commutative") {
    forAll { (lhs:Exp, rhs: Exp) =>
      min(lhs, rhs) must be(min(rhs, lhs))
    }
  }

  property("Min is associative") {
    forAll { (a: Exp, b: Exp, c: Exp) =>
      min(min(a, b), c) must be(min(a, min(b, c)))
    }
  }

  property("Monte Carlo equals isn't obviously broken") {
    forAll { (a: Exp, b: Exp) =>
      a.monteCarloEquals(a + b) must be(b.monteCarloEquals(Number(0)))
    }
  }
}

package cas

import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import org.scalatest.matchers.MustMatchers
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

class GenericExpressionTests extends PropSpec with PropertyChecks with MustMatchers {

  import ExpressionGenerators._

  implicit lazy val arbInteger: Arbitrary[Int] = Arbitrary(Gen.chooseNum(-10, 10))

  type Exp = MathExp[Name]

  implicit lazy val genOperator: Arbitrary[CasBinaryOperator[Name]] = Arbitrary(for {
    x <- Gen.option(Gen.const(Idempotent))
    y <- Gen.option(Gen.const(Associative))
    z <- Gen.option(Gen.const(Commutative))
  } yield new CasBinaryOperator(Name("f"), List(x, y, z).flatten.toSet))

  property("Min is commutative") {
    forAll { (a: Exp, b: Exp) =>
      min(a, b) must be(min(b, a))
    }
  }

  property("Min is associative") {
    forAll { (a: Exp, b: Exp, c: Exp) =>
      min(a, min(b, c)) must be(min(min(a, b), c))
    }
  }

  property("Functions are associative if they should be") {
    forAll { (f: CasBinaryOperator[Name], a: Exp, b: Exp, c: Exp) =>
      if (f.is(Associative)) {
        f(a, f(b, c)) must be(f(f(a, b), c))
      }
    }
  }

  property("Functions are not associative if they should not be") {
    forAll { (f: CasBinaryOperator[Name], a: Exp, b: Exp, c: Exp) =>
      if (!f.is(Associative) && a != b && b != c && a != c) {
        f(a, f(b, c)) must not be f(f(a, b), c)
      }
    }
  }

  property("Functions are commutative if they should be") {
    forAll { (f: CasBinaryOperator[Name], a: Exp, b: Exp) =>
      if (f.is(Commutative)) {
        f(a, b) must be(f(b, a))
      }
    }
  }

  property("Functions are not commutative if they should not be") {
    forAll { (f: CasBinaryOperator[Name], a: Exp, b: Exp) =>
      if (!f.is(Commutative) && a != b) {
        f(a, b) must not be f(b, a)
      }
    }
  }

  property("Functions are idempotent if they should be") {
    forAll { (f: CasBinaryOperator[Name], a: Exp) =>
      if (f.is(Idempotent)) {
        f(a, a) must be(a)
      }
    }
  }

  property("Functions are not idempotent if they should not be") {
    forAll { (f: CasBinaryOperator[Name], a: Exp) =>
      if (!f.is(Idempotent)) {
        f(a, a) must not be a
      }
    }
  }
}

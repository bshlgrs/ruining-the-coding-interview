package big_o

import java_transpiler.{JavaExpressionOrQuery, JavaVariable, JavaMethodCall, JavaExpression}

import com.github.javaparser.ast.expr.MethodCallExpr

case class BigO(powerOfN: Int, sqrtOfN: Boolean, powerOfLogN: Int) extends Ordered[BigO] {
  assert(powerOfN >= 0)
  assert(powerOfLogN >= 0)

  def compare(other: BigO): Int = (this, other) match {
    case (BigO(n1, _, _),BigO(n2, _, _)) if n1 != n2 => n1 - n2
    case (BigO(_, b1, _), BigO(_, b2, _)) if b1 != b2 => if (b1) 1 else -1
    case (BigO(_, _, n1), BigO(_, _, n2)) => n1 - n2
  }

  // this is a hack to make java interop nicer.
  def time() = this

  override def toString = {
    if (this == Constant)
      "O(1)"
    else {
      val firstStringList = powerOfN match {
        case 0 => Nil
        case 1 => List("n")
        case n => List(s"n**$powerOfN")
      }

      val secondStringList = if (sqrtOfN)
        List(s"sqrt(n)")
      else
        Nil

      val thirdStringList = powerOfLogN match {
        case 0 => Nil
        case 1 => List("log n")
        case n => List(s"(log n)**$powerOfN")
      }

      "O(" + List(firstStringList, secondStringList, thirdStringList).flatten.mkString(" ") + ")"
    }
  }

  def *(other: BigO): BigO = {
    val extraThing = if (this.sqrtOfN && other.sqrtOfN) 1 else 0
    BigO(
      this.powerOfN + other.powerOfN + extraThing,
      this.sqrtOfN != other.sqrtOfN,
      this.powerOfLogN + other.powerOfLogN)
  }
}

object BigO {
  def fromJavaExpression(time: JavaExpressionOrQuery): BigO = time match {
    case JavaMethodCall(JavaVariable(name), "time", Nil) => name match {
      case "Constant" => Constant
      case "Linear" => Linear
      case "Logarithmic" => Logarithmic
      case "Linearithmic" => Linearithmic
      case "Quadratic" => Quadratic
    }
    case _ => throw new RuntimeException(s"$time is not a BigO.")
  }

}

object Linear extends BigO(1, false, 0)
object Quadratic extends BigO(2, false, 0)
object Logarithmic extends BigO(0, false, 1)
object Linearithmic extends BigO(1, false, 1)
object Constant extends BigO(0, false, 0)


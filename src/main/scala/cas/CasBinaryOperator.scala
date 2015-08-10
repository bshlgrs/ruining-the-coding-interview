package cas

class CasBinaryOperator[A](val name: Name,
                           val properties: Set[OperatorProperty],
                           val identities: List[MathExp[A]] = List(),
                           val annihilator: Option[MathExp[A]] = None) {
  def apply(lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = lhs.applyBinaryOperator(this, rhs)

  override def toString = s"$name[${properties.mkString(",")}]"

  def lossilyConvert[B](): CasBinaryOperator[B] = new CasBinaryOperator[B](name, properties)

  // this deals with the case when you're taking the min of two expressions which aren't mins themselves.
  def seedWithOperation(lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = {
    (is(Commutative), is(Associative), is(Idempotent)) match {
      case (true, true, true) => SetApplication[A](this, Set(lhs, rhs)).perhapsDeflate()
      case (true, true, false) => this.apply(MultisetApplication[A](this, new Multiset[MathExp[A]](Map(lhs -> 1))), rhs)
      case (true, false, true) =>
        if (lhs == rhs)
          lhs
        else if (lhs.hashCode() < rhs.hashCode())
          SymmetricIdempotentTreeApplication[A](this, lhs, rhs)
        else
          SymmetricIdempotentTreeApplication[A](this, rhs, lhs)
      case (true, false, false) =>
        if (lhs.hashCode() < rhs.hashCode())
          SymmetricTreeApplication[A](this, lhs, rhs)
        else
          SymmetricTreeApplication[A](this, rhs, lhs)
      case (false, true, true) =>
        if (lhs == rhs)
          lhs
        else
          NoDuplicatesListApplication[A](this, List(lhs, rhs))
      case (false, true, false) => ListApplication[A](this, List(lhs, rhs))
      case (false, false, true) =>
        if (lhs == rhs)
          lhs
        else
          IdempotentTreeApplication[A](this, lhs, rhs)
      case (false, false, false) => BinaryTreeApplication(this, lhs, rhs)
  }}

  def is(props: OperatorProperty*): Boolean = props.forall(properties.contains)

  def isIdentity(a: MathExp[A]) = identities contains a

  def isAnnihilator(a: MathExp[A]) = annihilator.toList contains a
}

sealed abstract class OperatorProperty
case object Commutative extends OperatorProperty
case object Associative extends OperatorProperty
case object Idempotent extends OperatorProperty

object min {
  case class operator[A]() extends CasBinaryOperator[A](Name("min"), Set(Commutative, Associative, Idempotent))

  def apply[A](lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = operator()(lhs, rhs)
}

object max {
  case class operator[A]() extends CasBinaryOperator[A](Name("max"), Set(Commutative, Associative, Idempotent))

  def apply[A](lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = operator()(lhs, rhs)
}

object bitwiseAnd {
  case class operator[A]() extends CasBinaryOperator[A](Name("&"), Set(Commutative, Associative, Idempotent))

  def apply[A](lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = operator()(lhs, rhs)
}

object logicalAnd {
  case class operator[A]() extends CasBinaryOperator[A](Name("&"), Set(Commutative, Associative, Idempotent))

  def apply[A](lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = operator()(lhs, rhs)
}

object logicalOr {
  case class operator[A]() extends CasBinaryOperator[A](Name("&"), Set(Commutative, Associative, Idempotent))

  def apply[A](lhs: MathExp[A], rhs: MathExp[A]): MathExp[A] = operator()(lhs, rhs)
}


//object max extends CasBinaryOperator(Name("min"), List(Commutative[Any], Associative[Any], Idempotent[Any]))

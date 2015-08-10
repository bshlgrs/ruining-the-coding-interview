package cas

abstract class BinaryOperatorApplication[A](val operator: CasBinaryOperator[A]) extends MathExp[A] {
  def combineWithCollection(other: this.type): MathExp[A]
  def leftCombineWithItem(other: MathExp[A]): MathExp[A]
  def rightCombineWithItem(other: MathExp[A]): MathExp[A]
  def perhapsDeflate(): MathExp[A]
  def postSimplify(): MathExp[A] = this
}

case class SetApplication[A](op: CasBinaryOperator[A], set: Set[MathExp[A]]) extends BinaryOperatorApplication[A](op) {
  assert(op.is(Commutative, Associative, Idempotent))

  lazy val variables = set.flatMap(_.variables)

  def mapOverVariables[B](f: A => B): MathExp[B] = set.map(_.mapOverVariables(f)).reduce(op.lossilyConvert[B]()(_, _))

  def substitute(map: Map[A, MathExp[A]]): MathExp[A] = SetApplication[A](op, set.map(_.substitute(map)))

  def combineWithCollection(other: this.type): MathExp[A] = {
    assert(this.op == other.op)
    SetApplication(op, set ++ other.set).asInstanceOf[MathExp[A]]
  }

  def leftCombineWithItem(item: MathExp[A]) = {
    SetApplication(op, set + item).perhapsDeflate()
  }

  def rightCombineWithItem(item: MathExp[A]) = leftCombineWithItem(item)

  def perhapsDeflate() = this.set.size match {
    case 0 => ???
    case 1 => this.set.toList.head
    case _ => this
  }
}

case class ListApplication[A](op: CasBinaryOperator[A], list: List[MathExp[A]]) extends BinaryOperatorApplication[A](op) {
  lazy val variables = list.flatMap(_.variables).toSet

  def mapOverVariables[B](f: A => B): MathExp[B] = list.map(_.mapOverVariables(f)).reduce(op.lossilyConvert[B]()(_, _))

  def substitute(map: Map[A, MathExp[A]]): MathExp[A] = list.map(_.substitute(map)).reduce(op.apply)

  def combineWithCollection(other: this.type): MathExp[A] = {
    assert(this.op == other.op)
    ListApplication(op, list ++ other.list).asInstanceOf[MathExp[A]]
  }

  def leftCombineWithItem(item: MathExp[A]) = {
    ListApplication(op, item +: list).perhapsDeflate()
  }

  def rightCombineWithItem(item: MathExp[A]) = { ListApplication(op, list :+ item).perhapsDeflate() }

  def perhapsDeflate() = this.list.size match {
    case 0 => ???
    case 1 => this.list.head
    case _ => this
  }
}

case class MultisetApplication[A](op: CasBinaryOperator[A], multiset: Multiset[MathExp[A]])
  extends BinaryOperatorApplication[A](op) {
  lazy val variables = multiset.keys.flatMap(_.variables).toSet

  def mapOverVariables[B](f: A => B): MathExp[B] =
    multiset.splitToMultisets.map((x: Multiset[MathExp[A]]) =>
      MultisetApplication(
        op.lossilyConvert[B](),
        Multiset[MathExp[B]](Map(x.items.head._1.mapOverVariables(f) -> x.items.head._2)))
    ).asInstanceOf[List[MathExp[B]]].reduce(op.lossilyConvert[B]()(_, _))

  def substitute(map: Map[A, MathExp[A]]): MathExp[A] = {
    val itemsList: List[MathExp[A]] = multiset.splitToMultisets.map({x: Multiset[MathExp[A]] =>
      val (exp, number) = x.items.head
      new MultisetApplication[A](op, Multiset[MathExp[A]](Map(exp.substitute(map) -> number)))
    })

    itemsList.tail.foldLeft(itemsList.head){ (x: MathExp[A], y: MathExp[A]) => op(x, y) }
  }

  def combineWithCollection(other: this.type): MathExp[A] = {
    assert(this.op == other.op)
    MultisetApplication[A](op, multiset.combine(other.multiset))
  }

  def leftCombineWithItem(item: MathExp[A]) = {
    MultisetApplication[A](op, multiset.add(item))
  }

  def rightCombineWithItem(item: MathExp[A]) = leftCombineWithItem(item)

  def perhapsDeflate() = this.multiset.size match {
    case 0 => ???
    case 1 => multiset.items.head._1
    case _ => this
  }
}

case class NoDuplicatesListApplication[A](op: CasBinaryOperator[A], list: List[MathExp[A]])
  extends BinaryOperatorApplication[A](op) {
  lazy val variables = list.flatMap(_.variables).toSet

  def mapOverVariables[B](f: A => B): MathExp[B] = list.map(_.mapOverVariables(f)).reduce(op.lossilyConvert[B]()(_, _))

  def substitute(map: Map[A, MathExp[A]]): MathExp[A] = {
    def removeConsecutiveDuplicates(list: List[MathExp[A]]): List[MathExp[A]] = list match {
      case x :: y :: xs if x == y => removeConsecutiveDuplicates(y :: xs)
      case x :: xs => x :: removeConsecutiveDuplicates(xs)
      case xs => xs
    }

    NoDuplicatesListApplication(op, removeConsecutiveDuplicates(list.map(_.substitute(map))))
  }

  def combineWithCollection(other: this.type): MathExp[A] = {
    assert(this.op == other.op)
    NoDuplicatesListApplication[A](op, list ++ other.list.dropWhile(_ == list.last))
  }

  def leftCombineWithItem(item: MathExp[A]) = {
    if (item == list.head) {
      this
    }
    else {
      NoDuplicatesListApplication(op, item :: list)
    }
  }

  def rightCombineWithItem(item: MathExp[A]) = {
    if (item == list.last) {
      this
    }
    else {
      NoDuplicatesListApplication(op, list ++ List(item))
    }
  }

  def perhapsDeflate() = list.size match {
    case 0 => ???
    case 1 => list.head
    case _ => this
  }
}

case class BinaryTreeApplication[A](op: CasBinaryOperator[A], lhs: MathExp[A], rhs: MathExp[A]) extends BinaryOperatorApplication[A](op) {
  lazy val variables = lhs.variables ++ rhs.variables

  def mapOverVariables[B](f: A => B): MathExp[B] = op.lossilyConvert[B]()(lhs.mapOverVariables(f), rhs.mapOverVariables(f))

  def substitute(map: Map[A, MathExp[A]]) = op.apply(lhs.substitute(map), rhs.substitute(map))

  def combineWithCollection(other: this.type) = {
    assert(this.op == other.op)
    BinaryTreeApplication(op, this, other)
  }

  def leftCombineWithItem(item: MathExp[A]) = BinaryTreeApplication(op, this, item)
  def rightCombineWithItem(item: MathExp[A]) = BinaryTreeApplication(op, item, this)
  def perhapsDeflate() = this
}

case class SymmetricTreeApplication[A](op: CasBinaryOperator[A], lhs: MathExp[A], rhs: MathExp[A]) extends BinaryOperatorApplication[A](op) {
  assert(lhs.hashCode() <= rhs.hashCode(), "ordering is violated for SymmetricIdempotentTreeApplication")

  lazy val variables = lhs.variables ++ rhs.variables

  def mapOverVariables[B](f: A => B): MathExp[B] = op.lossilyConvert[B]()(lhs.mapOverVariables(f), rhs.mapOverVariables(f))

  def substitute(map: Map[A, MathExp[A]]) = op.apply(lhs.substitute(map), rhs.substitute(map))

  def combineWithCollection(other: this.type) = {
    assert(this.op == other.op)
    if (this.hashCode() < other.hashCode())
      BinaryTreeApplication(op, this, other)
    else
      BinaryTreeApplication(op, other, this)
  }

  def leftCombineWithItem(item: MathExp[A]) = {
    if (this.hashCode() < item.hashCode())
      BinaryTreeApplication(op, this, item)
    else
      BinaryTreeApplication(op, item, this)
  }
  def rightCombineWithItem(item: MathExp[A]) = BinaryTreeApplication(op, item, this)
  def perhapsDeflate() = this
}

case class SymmetricIdempotentTreeApplication[A](op: CasBinaryOperator[A], lhs: MathExp[A], rhs: MathExp[A]) extends BinaryOperatorApplication[A](op) {
  assert(lhs.hashCode() < rhs.hashCode(), "ordering is violated for SymmetricIdempotentTreeApplication")

  lazy val variables = lhs.variables ++ rhs.variables

  def mapOverVariables[B](f: A => B): MathExp[B] = op.lossilyConvert[B]()(lhs.mapOverVariables(f), rhs.mapOverVariables(f))

  def substitute(map: Map[A, MathExp[A]]) = op.apply(lhs.substitute(map), rhs.substitute(map))

  def combineWithCollection(other: this.type) = {
    assert(this.op == other.op)
    if (this == other)
      this
    else if (this.hashCode() < other.hashCode())
      BinaryTreeApplication(op, this, other)
    else
      BinaryTreeApplication(op, other, this)
  }

  def leftCombineWithItem(item: MathExp[A]) = {
    if (this == item)
      this
    else if (this.hashCode() < item.hashCode())
      BinaryTreeApplication(op, this, item)
    else
      BinaryTreeApplication(op, item, this)
  }
  def rightCombineWithItem(item: MathExp[A]) = BinaryTreeApplication(op, item, this)
  def perhapsDeflate() = this
}

case class IdempotentTreeApplication[A](op: CasBinaryOperator[A], lhs: MathExp[A], rhs: MathExp[A]) extends BinaryOperatorApplication[A](op) {
  assert(lhs != rhs, "invariant violated in IdempotentTreeApplication")

  lazy val variables = lhs.variables ++ rhs.variables

  def mapOverVariables[B](f: A => B): MathExp[B] = op.lossilyConvert[B]()(lhs.mapOverVariables(f), rhs.mapOverVariables(f))

  def substitute(map: Map[A, MathExp[A]]) = op.apply(lhs.substitute(map), rhs.substitute(map))

  def combineWithCollection(other: this.type) = rightCombineWithItem(other)

  def leftCombineWithItem(item: MathExp[A]) = {
    if (this == item)
      this
    else
      IdempotentTreeApplication(op, item, this)
  }

  def rightCombineWithItem(item: MathExp[A]) = {
    if (this == item)
      this
    else
      IdempotentTreeApplication(op, this, item)
  }
  def perhapsDeflate() = this
}

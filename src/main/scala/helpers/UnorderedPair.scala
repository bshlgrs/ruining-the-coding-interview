package helpers

case class UnorderedPair[A](lesser: A, greater: A) {
  assert(lesser.hashCode() < greater.hashCode())

  lazy val toSet = Set(lesser, greater)

  lazy val toList = List(lesser, greater)
}

object UnorderedPair {
  def build[A](first: A, second: A): UnorderedPair[A] = {
    if (first.hashCode() < second.hashCode())
      UnorderedPair(first, second)
    else
      UnorderedPair(second, first)
  }
}

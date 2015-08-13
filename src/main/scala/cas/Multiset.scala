package cas

case class Multiset[A](items: Map[A, Int]) {
  def get(item: A) = items.getOrElse(item, 0)

  def add(item: A): Multiset[A] = {
    Multiset(items + (item -> (get(item) + 1)))
  }

  def addMultiple(item: A, number: Int): Multiset[A] = {
    Multiset(items + (item -> (get(item) + number)))
  }

  def combine(other: Multiset[A]) = {
    other.items.foldLeft(this) { (multiset: Multiset[A], tuple: (A, Int)) =>
      multiset.addMultiple(tuple._1, tuple._2)
    }
  }

  def splitToTwoChildren = {
    items.head match {
      case (item, 1) => (item, Multiset(items.tail))
      case (item, n) => (item, Multiset(Map(item -> (n - 1)) ++ items.tail))
    }
  }

  lazy val keys = items.keys

  lazy val splitToMultisets: List[Multiset[A]] = items.map({x: (A, Int) => Multiset(Map(x._1 -> x._2))}).toList

  lazy val size = items.values.sum
}

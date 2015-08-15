package helpers


object VariableNameGenerator {
  var badVariableNames: List[String] = "foo bar cat dog hamster bird knife happy sad foolish game ruby x y z thing huh".split(" ").toList

  def getVariableName(): String = {
    val result = badVariableNames.head
    badVariableNames = badVariableNames.tail ++ List(result)
    result
  }

  def getRandomName(): String = {
    "x" + randomString(10)
  }

  def randomString(length: Int) = Stream.continually(util.Random.nextPrintableChar) take length mkString
}

package helpers


object VariableNameGenerator {
  var badVariableNames: List[String] = "foo bar cat dog hamster bird knife happy sad foolish".split(" ").toList

  def getVariableName(): String = {
    val result = badVariableNames.head
    badVariableNames = badVariableNames.tail
    result
  }
}

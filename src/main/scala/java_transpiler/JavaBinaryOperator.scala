package java_transpiler

sealed abstract class JavaBinaryOperator(name: String)

case object JavaPlus extends JavaBinaryOperator("+")
case object JavaMinus extends JavaBinaryOperator("-")
case object JavaTimes extends JavaBinaryOperator("*")

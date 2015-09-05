package languages

import java_transpiler.JavaExpression

sealed abstract class MethodImplementation[A]

case class ExpressionImplementation[A](impl: JavaExpression[A]) extends MethodImplementation[A]

package languages

import java_transpiler.JavaExpression

abstract class LibraryModule[Language] {
  type methodImplementation = JavaExpression[Language]
}

package languages

abstract class StandardLibrary[A <: Language] {
  val arrays: ArrayImplementation[Language]
}

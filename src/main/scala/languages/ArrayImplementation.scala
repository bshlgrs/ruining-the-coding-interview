package languages

abstract class ArrayImplementation[A <: Language] extends LibraryModule[Language] {
  val get: methodImplementation
  val set: methodImplementation
  // push
  // pop
  // find
  // contains?
  // for each
  // filter
  // reduce
}

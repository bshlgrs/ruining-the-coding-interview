package languages

abstract class Language {
  val stdLib: StandardLibrary[this.type]
}

package data_structure_handlers

import java_transpiler._

case class ChimeraMultisetClass(components: List[GenericDataStructureForMultiset]) {
  lazy val toJavaClass: JavaClass = {

    val insertImplementations = components.map(_.insertion).collect({case Some(x) => x})

    val insert = MutatingMethodImplementation.buildMethod("insert", List("item" -> JavaIntType),
      insertImplementations, Nil)

    val removeImplementations = components.map(_.removal).collect({case Some(x) => x})

    val remove = MutatingMethodImplementation.buildMethod("remove", List("item" -> JavaIntType),
      removeImplementations, Nil)

    JavaClass("Chimera", None, Nil, List(insert, remove), Nil)
  }
}

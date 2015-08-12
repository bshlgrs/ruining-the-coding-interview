package java_transpiler

import java_transpiler.queries.UnorderedQuery

import useful_data_structures.UsefulUnorderedDataStructure

case class MagicMultiset(itemClass: JavaClass, supportsInsert: Boolean, supportsRemove: Boolean) {
  val itemType = itemClass.toType

  def modificationMethods(name: String,
                          auxillaryDataStructures: Map[UnorderedQuery, Option[UsefulUnorderedDataStructure]]): List[JavaMethodDeclaration] = {

    val deleteMethod = if (supportsRemove) {
      val removeBody = auxillaryDataStructures.collect({
        case (query, Some(dataStructure)) => query.source match {
          case JavaVariable(querySourceName) if name == querySourceName =>
            dataStructure.onRemove.get
        }
      }).flatten.toList

      val actualRemoval = JavaStatement.parse(s"this.$name.remove(item);")

      Some(JavaMethodDeclaration(
        s"removeFrom_$name",
        Some(JavaIntType),
        false,
        List("item" -> itemType),
        removeBody :+ actualRemoval))

    } else
      None

    val insertMethod = if (supportsInsert) {
      val insertBody = auxillaryDataStructures.collect({
        case (query, Some(dataStructure)) => query.source match {
          case JavaVariable(querySourceName) if name == querySourceName =>
            dataStructure.onInsert.get
        }
      }).flatten.toList

      val actualInsertion = ExpressionStatement(
        JavaMethodCall(
          JavaFieldAccess(JavaThis, name),
          "insert",
          itemClass.fields.map((x) => JavaVariable(x.name))))

      val insertArgs = itemClass.fields.map((x) => x.name -> x.javaType)

      Some(JavaMethodDeclaration(s"insertInto_$name", Some(itemType), false, insertArgs, insertBody :+ actualInsertion))
    } else
      None


    List(deleteMethod, insertMethod).flatten
  }
}

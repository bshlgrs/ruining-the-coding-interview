package useful_data_structures

import java_transpiler._
import java_transpiler.queries._

import big_o.BigO
import useful_data_structures.data_structure_library.MonoidMemoizerFactory

object UnorderedDataStructureLibrary {
  val helpfulStructures: List[UsefulUnorderedDataStructureFactory] = List(
    MonoidMemoizerFactory
  )

  def getBestStructureForClass(query: UnorderedQuery, javaClass: JavaClass): Option[UsefulUnorderedDataStructure] = {
    // obviously this is hella dangerous
    val multiset = javaClass.magicMultisets(query.source.asInstanceOf[JavaVariable].name)
    getBestStructure(query, multiset.supportsInsert, multiset.supportsRemove)
  }

  def getBestStructure(query: UnorderedQuery,
                       requiresInsert: Boolean,
                       requiresDelete: Boolean): Option[UsefulUnorderedDataStructure] = {
    helpfulStructures
      .flatMap { _.tryToCreate(query) }
      .filter(_._1.insertionFragment.isDefined || ! requiresInsert)
      .filter(_._1.removalFragment.isDefined || ! requiresDelete)
      .sortBy(_._2)
      .headOption
      .map(_._1)
  }
}

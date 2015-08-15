package useful_data_structures.data_structure_library

import java_transpiler._
import java_transpiler.queries.{UnorderedQuery, WhereClause}

import big_o.{BigO, Constant}
import helpers.VariableNameGenerator
import useful_data_structures._

object GroupMemoizerFactory extends UsefulUnorderedDataStructureFactory {

  case class GroupMemoizer(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
    val asymptoticQueryTime = Constant

    val reduction = query.mbReduction.get

    val fieldName = VariableNameGenerator.getVariableName()

    lazy val insertionFragment: Option[List[JavaStatement]] = {
      val mapper = reduction.mapper

      val variableMap = Map(
        reduction.reducer.arg1 -> getField(fieldName),
        reduction.reducer.arg2 -> mapper.useStr("item")
      )

      val body = reduction.reducer.useBody(variableMap)

      Some(List(setField(fieldName, body)))
    }

    def removalFragment: Option[List[JavaStatement]] = {
      val mapper = reduction.mapper
      val reducer = reduction.invertedReducer.get

      val variableMap = Map(
        reducer.arg1 -> getField(fieldName),
        reducer.arg2 -> mapper.useStr("item")
      )

      val body = reducer.useBody(variableMap)

      Some(List(setField(fieldName, body)))
    }

    def fieldFragments = List(JavaFieldDeclaration(fieldName, JavaIntType, Some(reduction.start)))

    def queryCode = getField(fieldName)

    def methodCode = None
  }

  def tryToCreate(query: UnorderedQuery): Option[UsefulUnorderedDataStructure] = {
    query match {
      case UnorderedQuery(source, whereClauses, None, Some(reduction))
        if query.trickyWhereClauses.size == 0 && reduction.isInvertible =>
        Some(GroupMemoizer(query))
      case _ => None
    }
  }
}

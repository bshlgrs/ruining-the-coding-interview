package useful_data_structures.data_structure_library

import java_transpiler._
import java_transpiler.queries.{UnorderedQuery, WhereClause}

import big_o.{BigO, Constant}
import helpers.VariableNameGenerator
import useful_data_structures.{UsefulDataStructureHelper, UsefulUnorderedDataStructure, UsefulUnorderedDataStructureFactory}

object MonoidMemoizerFactory extends UsefulUnorderedDataStructureFactory {
  case class MonoidMemoizer(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
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

      Some(List(ExpressionStatement(JavaAssignmentExpression(fieldName, false, body))))
    }

    def removalFragment: Option[List[JavaStatement]] = None

    def fieldFragments = List(JavaFieldDeclaration(fieldName, JavaIntType, Some(reduction.start)))

    def queryCode = JavaFieldAccess(JavaThis, fieldName)

    def methodCode = None
  }

  def tryToCreate(query: UnorderedQuery): Option[UsefulUnorderedDataStructure] = {
    query match {
      case UnorderedQuery(source, whereClauses, None, Some(reduction))
        if query.trickyWhereClauses.size == 0 =>
        Some(MonoidMemoizer(query))
      case _ => None
    }
  }
}

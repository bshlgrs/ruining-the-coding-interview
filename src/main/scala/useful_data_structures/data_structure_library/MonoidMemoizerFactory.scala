package useful_data_structures.data_structure_library

import java_transpiler._
import java_transpiler.queries.{UnorderedQuery, WhereClause}

import big_o.{BigO, Constant}
import helpers.VariableNameGenerator
import useful_data_structures.{UsefulDataStructureHelper, UsefulUnorderedDataStructure, UsefulUnorderedDataStructureFactory}

object MonoidMemoizerFactory extends UsefulUnorderedDataStructureFactory {
  case class MonoidMemoizer(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
    val whereClauses: List[WhereClause] = query.whereClauses
    val reduction = query.mbReduction.get

    val variableName = VariableNameGenerator.getVariableName()
    val methodName = s"get_$variableName"

    lazy val onInsert: Option[List[JavaStatement]] = {
      val mapper = reduction.mapper

      val variableMap = Map(
        reduction.reducer.arg1 -> JavaFieldAccess(JavaThis, variableName),
        reduction.reducer.arg2 -> mapper.useStr("item")
      )

      val body = reduction.reducer.useBody(variableMap)

      val assignment = ExpressionStatement(JavaAssignmentExpression(variableName, false, body))

      UsefulDataStructureHelper.wrapInConstantWheres(whereClauses, List(assignment))
    }

    def onDelete = None

    def fields = List(JavaFieldDeclaration(variableName, JavaIntType, Some(reduction.start)))

    def queryCode = JavaFieldAccess(JavaThis, variableName)

    def methodCode = None
  }

  def tryToCreate(query: UnorderedQuery): Option[(UsefulUnorderedDataStructure, BigO)] = {
    query match {
      case UnorderedQuery(source, whereClauses, None, Some(reduction))
        if whereClauses.forall(_.constantWhereClause.isDefined) =>
        Some(MonoidMemoizer(query), Constant)
      case _ => None
    }
  }
}

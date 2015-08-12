package useful_data_structures.data_structure_library

import java_transpiler._
import java_transpiler.queries.{WhereClause, UnorderedQuery}

import big_o.{Constant, BigO}
import helpers.VariableNameGenerator
import useful_data_structures._

//class PriorityQueueFactory extends UsefulUnorderedDataStructureFactory {
//  case class PriorityQueue(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
//    val whereClauses: List[WhereClause] = query.whereClauses
//    val reduction = query.mbReduction.get
//
//    val variableName = VariableNameGenerator.getVariableName()
//    val methodName = s"get_$variableName"
//
//    lazy val onInsert: Option[List[JavaStatement]] = {
//      val mapper = reduction.mapper
//      val mapperBody = mapper.body.replaceVariables(Map(mapper.arg -> JavaVariable("item")))
//
//      val variableMap = Map(reduction.reducer.arg1 -> JavaFieldAccess(JavaThis, variableName),
//        reduction.reducer.arg2 -> mapperBody)
//
//      val body = reduction.reducer.body.replaceVariables(variableMap)
//
//      val assignment = ExpressionStatement(JavaAssignmentExpression(variableName, false, body))
//
//      UsefulDataStructureHelper.wrapInConstantWheres(whereClauses, List(assignment))
//    }
//
//    def onDelete = None
//
//    def fields = List(JavaFieldDeclaration(variableName, JavaIntType, Some(reduction.start)))
//
//    def queryCode = JavaFieldAccess(JavaThis, variableName)
//
//    def methodCode = None
//  }
//
//  def tryToCreate(query: UnorderedQuery): Option[(UsefulUnorderedDataStructure, BigO)] = {
//    query match {
//      case UnorderedQuery(source, whereClauses, None, Some(reduction))
//        if whereClauses.forall(_.constantWhereClause.isDefined) =>
//        Some(PriorityQueue(query), Constant)
//      case _ => None
//    }
//  }
//}
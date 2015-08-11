package useful_data_structures

import java_transpiler._
import java_transpiler.queries._

import big_o.{Constant, BigO}
import cas.Number

abstract class UsefulUnorderedDataStructureFactory {
  def tryToCreate(query: UnorderedQuery): Option[(UsefulUnorderedDataStructure, BigO)]
}

abstract class UsefulUnorderedDataStructure(query: UnorderedQuery) {
  def onInsert: Option[List[JavaStatement]]
  def onDelete: Option[List[JavaStatement]]
  def fields: List[JavaFieldDeclaration]
  def queryCode: JavaExpressionOrQuery
  def methodCode: JavaMethodDeclaration
}

object MonoidMemoizerFactory extends UsefulUnorderedDataStructureFactory {
  case class MonoidMemoizer(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
    val whereClauses: List[ConstantWhereClause] = query.whereClauses.map(_.constantWhereClause.get)
    val reduction = query.reduction.get

    val variableName = s"what_${query.hashCode()}"
    val methodName = s"get_$variableName"

    // I don't know how to choose the second argument for this thing.

    lazy val onInsert = {
      val mapper = reduction.mapper
      val mapperBody = mapper.body.replaceVariables(Map(mapper.arg -> JavaVariable("node")))

      val variableMap = Map(reduction.reducer.arg1 -> JavaVariable(variableName),
                            reduction.reducer.arg2 -> mapperBody)

      val body = reduction.reducer.body.replaceVariables(variableMap)

      Some(List(ExpressionStatement(JavaAssignmentExpression(variableName, false,
        reduction.reducer.body.replaceVariables(Map())))))
    }

    def onDelete = None

    def fields = List(JavaFieldDeclaration(variableName, JavaIntType, Some(reduction.start)))

    def queryCode = JavaMethodCall(JavaThis, methodName, Nil)

    def methodCode = JavaMethodDeclaration(methodName, Some(JavaIntType), false, Nil,
      List(ReturnStatement(JavaVariable(variableName))))
  }

  def tryToCreate(query: UnorderedQuery): Option[(UsefulUnorderedDataStructure, BigO)] = query match {
    case UnorderedQuery(source, whereClauses, None, Some(reduction))
      if whereClauses.forall(_.constantWhereClause.isDefined) =>
        Some(MonoidMemoizer(query), Constant)
    case _ => None
  }
}




package useful_data_structures

import java_transpiler._
import java_transpiler.queries.{WhereClause, UnorderedQuery}

import big_o.{Constant, BigO}
import helpers.VariableNameGenerator

object MonoidMemoizerFactory extends UsefulUnorderedDataStructureFactory {
  case class MonoidMemoizer(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
    val whereClauses: List[WhereClause] = query.whereClauses
    val reduction = query.reduction.get

    val variableName = VariableNameGenerator.getVariableName()
    val methodName = s"get_$variableName"

    lazy val onInsert: Option[List[JavaStatement]] = {
      val mapper = reduction.mapper
      val mapperBody = mapper.body.replaceVariables(Map(mapper.arg -> JavaVariable("item")))

      val variableMap = Map(reduction.reducer.arg1 -> JavaFieldAccess(JavaThis, variableName),
        reduction.reducer.arg2 -> mapperBody)

      val body = reduction.reducer.body.replaceVariables(variableMap)

      val assignment = ExpressionStatement(JavaAssignmentExpression(variableName, false, body))

      whereClauses match {
        case Nil => Some(List(assignment))
        case _ => {
          val clauses: List[JavaExpressionOrQuery] = whereClauses.map({ (x) =>
            x.replaceTarget("item").toJavaExpression
          })

          Some(UsefulDataStructureHelper.wrapInIfs(clauses, List(assignment)))
        }
      }
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

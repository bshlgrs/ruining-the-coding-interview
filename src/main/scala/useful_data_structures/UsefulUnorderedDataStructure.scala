package useful_data_structures

import java_transpiler._
import java_transpiler.queries._

import big_o.{Constant, BigO}
import cas.{MathExp, logicalAnd, CasBinaryOperator, Number}

abstract class UsefulUnorderedDataStructureFactory {
  def tryToCreate(query: UnorderedQuery): Option[(UsefulUnorderedDataStructure, BigO)]
}

abstract class UsefulUnorderedDataStructure(query: UnorderedQuery) {
  def onInsert: Option[List[JavaStatement]]
  def onDelete: Option[List[JavaStatement]]
  def fields: List[JavaFieldDeclaration]
  def queryCode: JavaExpressionOrQuery
  def methodCode: Option[JavaMethodDeclaration]
}

object UsefulDataStructureHelper {
  def wrapInIfs(clauses: List[JavaExpressionOrQuery], action: List[JavaStatement]) = {
    clauses.foldRight(action) { (condition: JavaExpressionOrQuery, statement: List[JavaStatement]) =>
      List(IfStatement(condition, statement, Nil)).asInstanceOf[List[JavaStatement]]
    }
  }

  def wrapInConstantWheres(whereClauses: List[WhereClause], statements: List[JavaStatement]) = {
    whereClauses match {
      case Nil => Some(statements)
      case _ => {
        val clauses: List[JavaExpressionOrQuery] = whereClauses.map({ (x) =>
          x.replaceTarget("item").toJavaExpression
        })

        Some(UsefulDataStructureHelper.wrapInIfs(clauses, statements))
      }
    }
  }
}




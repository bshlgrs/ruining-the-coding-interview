package useful_data_structures

import java_transpiler._
import java_transpiler.queries._

import big_o.{Constant, BigO}
import cas.{MathExp, logicalAnd, CasBinaryOperator, Number}

abstract class UsefulUnorderedDataStructureFactory {
  def tryToCreate(query: UnorderedQuery): Option[(UsefulUnorderedDataStructure, BigO)]
}

abstract class UsefulUnorderedDataStructure(query: UnorderedQuery) {
  def onInsert: Option[List[JavaStatement]] = insertionFragment.map { (fragment) =>
    UsefulDataStructureHelper.wrapInWheres(query.whereClauses, fragment)
  }

  def onRemove: Option[List[JavaStatement]] = removalFragment.map { (fragment) =>
    UsefulDataStructureHelper.wrapInWheres(query.whereClauses, fragment)
  }

  def insertionFragment: Option[List[JavaStatement]]
  def removalFragment: Option[List[JavaStatement]]
  def fields: List[JavaFieldDeclaration]
  def queryCode: JavaExpressionOrQuery
  def methodCode: Option[JavaMethodDeclaration]
}



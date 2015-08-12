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
    UsefulDataStructureHelper.filterAndWrapInWheres(query.whereClauses, fragment)
  }

  def onRemove: Option[List[JavaStatement]] = removalFragment.map { (fragment) =>
    UsefulDataStructureHelper.filterAndWrapInWheres(query.whereClauses, fragment)
  }

  def insertionFragment: Option[List[JavaStatement]]
  def removalFragment: Option[List[JavaStatement]]
  def fields: List[JavaFieldDeclaration] //= {
//    val separableEqualsWhereClauses = query.whereClauses.filter((x) => x.isSeparable && ! x.isConstant)
//
//
//  }

//  def fieldFragments: List[(String, JavaType)]

  lazy val separableEqualsWhereClauses: List[SeparableEqualityWhereClause] = {
    val clauses = query.whereClauses.filter((x) => x.isEqualitySeparable && ! x.isConstant)
    clauses.map(_.separableEqualityWhereClause.get).toList
  }
  
  def getField(fieldName: String): JavaExpressionOrQuery = {
    separableEqualsWhereClauses match {
      case Nil => JavaFieldAccess(JavaThis, fieldName)
      case _ => ???
    }
  }
  
  
  
  def queryCode: JavaExpressionOrQuery
  def methodCode: Option[JavaMethodDeclaration]
}



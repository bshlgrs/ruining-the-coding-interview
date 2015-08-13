package useful_data_structures

import java_transpiler._
import java_transpiler.queries._

import big_o.{Constant, BigO}
import cas.{MathExp, logicalAnd, CasBinaryOperator, Number}

abstract class UsefulUnorderedDataStructureFactory {
  def tryToCreate(query: UnorderedQuery): Option[UsefulUnorderedDataStructure]
}

abstract class UsefulUnorderedDataStructure(query: UnorderedQuery) {
  def asymptoticQueryTime: BigO

  def onInsert: Option[List[JavaStatement]] = insertionFragment.map { (fragment) =>
    UsefulDataStructureHelper.filterAndWrapInWheres(query.whereClauses, fragment)
  }

  def onRemove: Option[List[JavaStatement]] = removalFragment.map { (fragment) =>
    UsefulDataStructureHelper.filterAndWrapInWheres(query.whereClauses, fragment)
  }

  def insertionFragment: Option[List[JavaStatement]]
  def removalFragment: Option[List[JavaStatement]]

  def fields: List[JavaFieldDeclaration] = {
    fieldFragments.map { (decl) =>
      JavaFieldDeclaration(decl.name, wrapType(separableEqualsWhereClauses.length, decl.javaType), decl.initialValue)
    }
  }

  private def wrapInIndexingCalls(clauses: List[SeparableEqualityWhereClause],
                                  finalTarget: JavaExpressionOrQuery): JavaExpressionOrQuery = {
    clauses match {
      case Nil => finalTarget
      case clause :: otherClauses =>
        JavaMethodCall(wrapInIndexingCalls(otherClauses, finalTarget), "[]", List(clause.paramFunction))
    }
  }

  private def wrapType(n: Int, javaType: JavaType): JavaType = n match {
    case 0 => javaType
    case _ => JavaClassType("Hash", List(JavaIntType, wrapType(n -1, javaType)))
  }

  def fieldFragments: List[JavaFieldDeclaration]

  lazy val separableEqualsWhereClauses: List[SeparableEqualityWhereClause] = {
    val clauses = query.whereClauses.filter((x) => x.isEqualitySeparable && ! x.isConstant)
    clauses.map(_.separableEqualityWhereClause.get).toList
  }

  def getField(fieldName: String): JavaExpressionOrQuery = {
    wrapInIndexingCalls(separableEqualsWhereClauses, JavaFieldAccess(JavaThis, fieldName))
  }

  def setField(fieldName: String, value: JavaExpressionOrQuery): JavaStatement = separableEqualsWhereClauses.length match {
    case 0 =>
      ExpressionStatement(JavaAssignmentExpression(fieldName, false, value))
    case _ =>
      val hashMap = wrapInIndexingCalls(separableEqualsWhereClauses.tail, JavaFieldAccess(JavaThis, fieldName))
      ExpressionStatement(JavaMethodCall(hashMap, "[]=", List(separableEqualsWhereClauses.head.paramFunction, value)))
  }

  def queryCode: JavaExpressionOrQuery
  def methodCode: Option[JavaMethodDeclaration]
}

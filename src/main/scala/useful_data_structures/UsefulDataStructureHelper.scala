package useful_data_structures

import java_transpiler.queries.WhereClause
import java_transpiler.{IfStatement, JavaStatement, JavaExpressionOrQuery}

object UsefulDataStructureHelper {
  def wrapInIfs(clauses: List[JavaExpressionOrQuery], action: List[JavaStatement]) = {
    clauses.foldRight(action) { (condition: JavaExpressionOrQuery, statement: List[JavaStatement]) =>
      List(IfStatement(condition, statement, Nil)).asInstanceOf[List[JavaStatement]]
    }
  }

  def wrapInWheres(whereClauses: Set[WhereClause], statements: List[JavaStatement]): List[JavaStatement] = {
    whereClauses.size match {
      case 0 => statements
      case _ => {
        val clauses: List[JavaExpressionOrQuery] = whereClauses.toList.map({ (x) =>
          x.replaceTarget("item").toJavaExpression
        })

        UsefulDataStructureHelper.wrapInIfs(clauses, statements)
      }
    }
  }

  def filterAndWrapInWheres(whereClauses: Set[WhereClause], statements: List[JavaStatement]): List[JavaStatement] = {
    wrapInWheres(whereClauses.filter((x) => x.isConstant || x.isSeparable), statements)
  }
}


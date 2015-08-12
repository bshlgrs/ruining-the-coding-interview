package java_transpiler

import java_transpiler.queries.UnorderedQuery
import useful_data_structures.UsefulUnorderedDataStructure

case class QueryActualizer(auxiliaryStructures: Map[UnorderedQuery, Option[UsefulUnorderedDataStructure]], javaClass: JavaClass) extends AstModifier {
  def stmtMapper(stmt: JavaStatement) = List(stmt)
  def exprMapper(expr: JavaExpressionOrQuery) = expr match {
    case UnorderedQueryApplication(query) =>
      auxiliaryStructures.get(query).flatten match {
        case Some(structure) => structure.queryCode
        case None => JavaVariable("SHIT_SHIT_SHIT")
      }
    case JavaMethodCall(JavaVariable(callee), "insert", args) =>
      if (javaClass.magicMultisets.keys.toSet.contains(callee))
        JavaMethodCall(JavaThis, s"insertInto_$callee", args)
      else
        expr
    case JavaMethodCall(JavaVariable(callee), "remove", args) =>
      if (javaClass.magicMultisets.keys.toSet.contains(callee))
        JavaMethodCall(JavaThis, s"removeFrom_$callee", args)
      else
        expr
    case _ => expr
  }
}

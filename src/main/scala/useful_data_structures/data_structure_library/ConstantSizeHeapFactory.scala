package useful_data_structures.data_structure_library

import java_transpiler._
import java_transpiler.queries.{WhereClause, UnorderedQuery}

import big_o.{Constant, BigO}
import helpers.VariableNameGenerator
import useful_data_structures._

import scala.collection.mutable

object ConstantSizeHeapFactory extends UsefulUnorderedDataStructureFactory {
  case class ConstantSizeHeap(query: UnorderedQuery) extends UsefulUnorderedDataStructure(query) {
    import JavaAbbreviations._

    val asymptoticQueryTime = Constant

    val limiter = query.mbLimiter.get
    val size = limiter.constantSizeLimitBy.get.size

    val fieldName = VariableNameGenerator.getVariableName()
    val field = getField(fieldName)

    lazy val insertionFragment: Option[List[JavaStatement]] = {
      val tempVarName = VariableNameGenerator.getVariableName()

      val precalculate = ExpressionStatement(
        JavaAssignmentExpression(tempVarName, true, limiter.applyOrderingFunction(item)))

      val sizeCondition = lt(field.call("length"), num(size))

      val condition = gt(field.call("min"), jv(tempVarName))

      val pop = ExpressionStatement(field.call("pop"))

      val maybePop = jif(
        sizeCondition, // if heap.length < maxHeapSize
        List(jif(condition,
          List(pop)
        ))
      )

      val push = ExpressionStatement(field.call("push", List(JavaArrayInitializerExpr(List(jv(tempVarName), item)))))

      Some(List(precalculate, maybePop, push))
    }

    def removalFragment: Option[List[JavaStatement]] = None

    def fieldFragments = List(
      JavaFieldDeclaration(fieldName,
      JavaClassType("MinHeap", List()),
      Some(JavaNewObject("MinHeap", Nil, List(limiter.limitingFunction))))
    )

    def queryCode = JavaFieldAccess(JavaThis, fieldName).call("keys")

    def methodCode = None
  }

  def tryToCreate(query: UnorderedQuery): Option[UsefulUnorderedDataStructure] = {
    query match {
      case UnorderedQuery(source, whereClauses, Some(limiter), None)
        if query.trickyWhereClauses.size == 0 && limiter.constantSizeLimitBy.isDefined =>
        Some(ConstantSizeHeap(query))
      case _ => None
    }
  }
}

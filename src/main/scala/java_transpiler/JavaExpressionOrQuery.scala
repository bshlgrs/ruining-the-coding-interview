package java_transpiler

import java_transpiler.queries.{JavaContext, UnorderedQuery}

// this class is implemented in JavaExpression and Query.
abstract class JavaExpressionOrQuery {
  def querify(javaContext: JavaContext): JavaExpressionOrQuery
}

case class UnorderedQueryApplication(unorderedQuery: UnorderedQuery) extends PeculiarExpression[Queries] {
  override def childrenExpressions() = unorderedQuery.childrenExpressions()

  def querify(javaContext: JavaContext) = this

  private def querifyMethodCall(callee: JavaExpression[A],
                                name: String,
                                args: List[JavaExpression[A]],
                                context: JavaContext): JavaExpression[A] = {
    name match {
      case "insert" => JavaMethodCall(callee, name, args)
      case "remove" => JavaMethodCall(callee, name, args)
      case _ => {
        val mbQuerifiedCallee: Option[UnorderedQuery] = callee match {
          case callee @ UnorderedQueryApplication[A](query) => Some(query)
          case JavaVariable(innerName) if context.unorderedMultisets.keys.toSet.contains(innerName) => {
            Some(UnorderedQuery.blank(callee))
          }
          case _ => None
        }

        mbQuerifiedCallee match {
          case None => JavaMethodCall(callee, name, args)
          case Some(querifiedCallee) => querifiedCallee.applyMethod(name, args, context)
        }
      }
    }
  }
}


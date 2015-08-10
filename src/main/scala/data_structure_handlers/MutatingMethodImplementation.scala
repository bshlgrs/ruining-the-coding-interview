package data_structure_handlers

import java_transpiler.{JavaType, JavaMethodDeclaration, JavaStatement}

import big_o.BigO

case class MutatingMethodImplementation(time: BigO, before: List[JavaStatement], after: List[JavaStatement])

object MutatingMethodImplementation {
  def buildMethod(name: String,
                  args: List[(String, JavaType)],
                  things: List[MutatingMethodImplementation],
                  core: List[JavaStatement]): JavaMethodDeclaration = {
    val body = things.flatMap(_.before) ++ core ++ things.flatMap(_.after)

    JavaMethodDeclaration(name, None, false, args, body)
  }

}

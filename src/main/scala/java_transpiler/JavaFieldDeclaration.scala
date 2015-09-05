package java_transpiler

import com.github.javaparser.ast.body._


case class JavaFieldDeclaration[A](name: String, javaType: JavaType[A], initialValue: Option[JavaExpression[A]] = None) {
  def modifyWithAstModifier[B](astModifier: AstModifier[A,B]): JavaFieldDeclaration[B] = {
    JavaFieldDeclaration(name, astModifier.modifyType(javaType), initialValue.map(astModifier.applyToExpr))
  }
}

object JavaFieldDeclaration {
  def build(fieldDeclaration: FieldDeclaration): JavaFieldDeclaration = {
    val list = fieldDeclaration.getChildrenNodes
    val javaType = JavaType.build(fieldDeclaration.getType)

    list.size match {
      case 2 => (list.get(0), list.get(1)) match {
        case (_type, dec: VariableDeclarator) =>
          JavaFieldDeclaration(dec.getId.getName, javaType, Option(dec.getInit).map(JavaExpression.build))
        case _ => ???
      }
      case _ => ???
    }
  }


}

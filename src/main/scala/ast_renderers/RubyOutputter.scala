package ast_renderers

import java.io.{File, PrintWriter}
import java_transpiler._

import external_interfaces.ExternalInterfaces

object RubyOutputter {
  def outputClassToFile(javaClass: JavaClass) = {
    val writer = new PrintWriter(new File(s"target/ruby/${javaClass.name}.rb" ))

    writer.write(outputClass(javaClass))
    writer.close()
  }

  def outputClass(javaClass: JavaClass): String = {
    val initializationStmts = javaClass.fields.collect({
      case x if x.initialValue.isDefined => ExpressionStatement(
        JavaAssignmentExpression(x.name, false, x.initialValue.get))
    })

    val initializationString = if (javaClass.constructor.isDefined || javaClass.fields.exists(_.initialValue.isDefined)) {
      val initializationMethod = JavaMethodDeclaration(
        "initialize",
        None,
        false,
        javaClass.constructor.map(_.args).getOrElse(Nil),
        javaClass.constructor.map(_.body).getOrElse(Nil) ++ initializationStmts)
      outputMethod(initializationMethod)
    }
    else
      ""

    val innerClasses = javaClass.innerClasses.map(outputClass).mkString("\n")

    val fields = javaClass.fields.map({ (x) =>
      s"# ${x.name}: ${x.javaType.toScalaTypeString()} = ${x.initialValue}"
    }).mkString("\n")

    val methodsString = javaClass.methods.map(outputMethod).mkString("\n\n")

    val badlyFormattedVersion = s"class ${javaClass.name}\n$innerClasses\n$initializationString\n$methodsString\nend"

    ExternalInterfaces.rubocopify(badlyFormattedVersion)
  }

  def outputMethod(decl: JavaMethodDeclaration): String = {
    val args = mbBracket(decl.args.map(_._1))

    val body = outputBlock(decl.body, true)

    s"def ${if (decl.isStatic) "self." else ""}${decl.name}$args\n$body\nend"
  }

  def outputStatement(stmt: JavaStatement, isAtEnd: Boolean): String = {
    val code = stmt match {
      case ExpressionStatement(exp) => outputExpression(exp)
      case ReturnStatement(exp) => isAtEnd match {
        case true => outputExpression(exp)
        case false => s"return ${outputExpression(exp)}"
      }
      case VariableDeclarationStatement(name, _, initialValue) => initialValue match {
        case Some(value) => "name = " + outputExpression(value)
        case None => ""
      }
      case IfStatement(cond, thenCase, elseCase) =>
        s"if ${outputExpression(cond)}\n${outputBlock(thenCase, isAtEnd)}\nelse\n${outputBlock(elseCase, isAtEnd)}\nend"
      case _ =>
        throw new RuntimeException(s"ruby needs to have a output for ${stmt.getClass}")
    }

    (isAtEnd, stmt) match {
      case (_, _: ReturnStatement) => code
      case (false, _) => code
      case (true, _) => code + "\nnil" // do I actually want this case?
    }
  }

  def outputBlock(stmts: List[JavaStatement], isAtEnd: Boolean): String = {
    val body = stmts.dropRight(1).map(outputStatement(_, false) + "\n").mkString("")
    val lastStatementInBody = stmts.lastOption.map(outputStatement(_, isAtEnd)).getOrElse("")
    body + "\n" + lastStatementInBody
  }

  def outputExpression(exp: JavaExpressionOrQuery): String = exp match {
    case JavaMath(ast) => ast.mapOverVariables(outputExpression).toString
    case JavaAssignmentExpression(name, isLocal, expr) =>
      isLocal match {
        case true => s"$name = ${outputExpression(expr)}"
        case false =>
          s"@$name = ${outputExpression(expr)}"
      }
    case JavaLambdaExpr(args, body) => args match {
      case Nil => throw new RuntimeException("Hey, I don't allow side effects in lambdas right now, so this is bad")
      case _ => s"lambda { |${args.map(_._1).mkString(", ")}| ${outputExpression(body)} }"
    }
    case JavaThis => "self"
    case JavaVariable(name) => name
    case JavaNewObject(className, _, args) => s"$className.new${mbBracket(args.map(outputExpression))}"
    case JavaArrayInitializerExpr(items) => "[" + items.map(outputExpression).mkString(", ") + "]"
    case JavaStringLiteral(x) => "\"" + x + "\""
    case JavaBoolLit(x) => x.toString
    case JavaFieldAccess(JavaThis, field) => s"@$field"
    case JavaFieldAccess(scope, field) => outputExpression(scope) + "." + field
    case JavaMethodCall(scope, field, args) => outputExpression(scope) + "." + field + mbBracket(args.map(outputExpression))
    case JavaNull => "nil"
    case UnorderedQueryApplication(query) => query.toString
  }

  def mbBracket(blah: List[String]) = {
    blah match {
      case Nil => ""
      case _ => "(" + blah.mkString(", ") + ")"
    }
  }
}

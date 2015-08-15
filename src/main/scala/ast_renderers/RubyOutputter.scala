package ast_renderers

import java.io.{File, PrintWriter}
import java_transpiler._

import cas._
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

    val innerClasses = ""// javaClass.innerClasses.map(outputClass).mkString("\n")

    val fields = javaClass.fields.map({ (x) =>
      s"# ${x.name}: ${x.javaType.toScalaTypeString()} = ${x.initialValue}"
    }).mkString("\n")

    val methodsString = javaClass.methods.map(outputMethod).mkString("\n\n")

    val badlyFormattedVersion = s"class ${javaClass.name}\n$innerClasses\n$initializationString\n$methodsString\nend"

    ExternalInterfaces.rubocopify(badlyFormattedVersion)
  }

  def outputMethod(decl: JavaMethodDeclaration): String = {
    val args = mbBracket(decl.args.map(_._1))

    val body = outputBlock(decl.body, true).split("\n+").mkString("\n")

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
        case Some(value) => s"$name = " + outputExpression(value)
        case None => ""
      }
      case IfStatement(cond, thenCase, elseCase) =>
        s"if ${outputExpression(cond)} \n ${outputBlock(thenCase, isAtEnd)}\nelse\n${outputBlock(elseCase, isAtEnd)}\nend"
      case _ =>
        throw new RuntimeException(s"ruby needs to have a output for ${stmt.getClass}")
    }

    (isAtEnd, stmt) match {
      case (_, _: ReturnStatement) => code
      case (false, _) => code
      case (true, _) => code// + "\nnil" // do I actually want this case?
    }
  }

  def outputBlock(stmts: List[JavaStatement], isAtEnd: Boolean): String = {
    val body = stmts.dropRight(1).map(outputStatement(_, false) + "\n").mkString("")
    val lastStatementInBody = stmts.lastOption.map(outputStatement(_, isAtEnd)).getOrElse("")
    body + "\n" + lastStatementInBody
  }

  def outputExpression(exp: JavaExpressionOrQuery): String = exp match {
    case JavaMath(ast) => outputMath(ast.mapOverVariables(outputExpression))
    case JavaAssignmentExpression(name, isLocal, expr) =>
      val variableString = if (isLocal) name else "@" + name
      val nameGetter = if (isLocal) JavaVariable(name) else JavaFieldAccess(JavaThis, name)

      expr match {
        case JavaMath(Sum(set)) if set.size == 2 && set.contains(JavaMathHelper.casify(nameGetter)) => {
          val otherThing = set.find(_ != JavaMathHelper.casify(nameGetter)).get

          s"$variableString += ${outputExpression(JavaMathHelper.decasify(otherThing))}"
        }
        case _ => s"$variableString = ${outputExpression(expr)}"
      }
    case JavaLambdaExpr(args, body) => args match {
      case Nil => body match {
        case JavaMath(Number(x)) => "{" + x.toString + "}"
//        case _ => s"lambda { ${outputExpression(body)} }"
        case _ => s"{ ${outputExpression(body)} }"
      }
      case _ =>
//        s"lambda { |${args.map(_._1).mkString(", ")}| ${outputExpression(body)} }"
        s"{ |${args.map(_._1).mkString(", ")}| ${outputExpression(body)} }"
    }
    case JavaThis => "self"
    case JavaVariable(name) => name
    case JavaNewObject(className, _, args) => ///s"$className.new${mbBracket(args.map(outputExpression))}"
      outputExpression(JavaMethodCall(JavaVariable(className), "new", args))
    case JavaArrayInitializerExpr(items) => "[" + items.map(outputExpression).mkString(", ") + "]"
    case JavaStringLiteral(x) => "\"" + x + "\""
    case JavaBoolLit(x) => x.toString
    case JavaFieldAccess(JavaThis, field) => s"@$field"
    case JavaFieldAccess(scope, field) => outputExpression(scope) + "." + field
    case JavaMethodCall(scope, methodName, args) => methodName match {
      case "[]" => outputExpression(scope) + "[" + outputExpression(args.head) + "]"
      case "[]=" => outputExpression(scope) + "[" + outputExpression(args.head) + "] = " + outputExpression(args.last)
      case _ =>
        args match {
          case Nil => outputExpression(scope) + "." + methodName
          case _ => args.last match {
            case x: JavaLambdaExpr =>
              outputExpression(JavaMethodCall(scope, methodName, args.dropRight(1))) + " " + outputExpression(x)
            case _ =>
              outputExpression(scope) + "." + methodName + mbBracket(args.map(outputExpression))
          }
        }
    }
    case JavaNull => "nil"
    case UnorderedQueryApplication(query) => query.toString
  }

  def outputMath(math: MathExp[String]): String = math match {
    case Sum(terms) => "(" + terms.map(outputMath).mkString(" + ") + ")"
    case CasVariable(thing) => thing
    case Product(terms) => "(" + terms.map(outputMath).mkString(" * ") + ")"
    case Number(n) => n.toString
    case Power(base, exp) => "(" + outputMath(base) + "**" + outputMath(exp) + ")"
    case BinaryTreeApplication(op, lhs, rhs) => s"(${outputMath(lhs)} ${op.toString} ${outputMath(rhs)})"
    case CasFunctionApplication(function, args) => function.toString match {
      case "==" => outputMath(args.head) + " == " + outputMath(args.last)
      case ">" => args.head match {
          // this is to ensure that you get things like x < 10 instead of 10 > x
        case Number(n) => outputMath(args.last) + " < " + outputMath(args.head)
        case _ =>  outputMath(args.head) + " > " + outputMath(args.last)
      }
    }
    case x: BinaryOperatorApplication[String] => x.operator.name.name match {
      case "^" => s"(${x.lhs} ^ ${x.rhs})"
    }
  }

  def mbBracket(blah: List[String]) = {
    blah match {
      case Nil => ""
      case _ => "(" + blah.mkString(", ") + ")"
    }
  }
}

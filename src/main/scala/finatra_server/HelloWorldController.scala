package finatra_server

import java_parser.JavaParserWrapper

import ast_renderers.RubyOutputter
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import useful_data_structures.UnorderedDataStructureLibrary

import scala.util.{Success, Failure, Try}

class HelloWorldController extends Controller {
  post("/compile") { compilationRequest: CompilationRequest =>
    val result = for {
      javaClass <- Try(JavaParserWrapper.parseJavaClassToAst(compilationRequest.contents))
      querified <- Try(javaClass.querify())
      auxiliaryDataStructures <- Try(querified.queries().map({ (x) =>
        x -> UnorderedDataStructureLibrary.getBestStructureForClass(x, querified)
      }).toMap)
      optimizedClass <- Try(querified.actualizeQueries(auxiliaryDataStructures))
      out <- Try(RubyOutputter.outputClass(optimizedClass))
    } yield out

    result match {
      case Success(out) => out
      case Failure(exception) => exception.toString
    }
  }

  get("/:*") { request: Request =>
    response.ok.file(
      request.params("*"))
  }
}

package org.enso.interpreter.instrument.command

import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that gathers info required for suggestion import.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for suggestion import
  */
final class ImportSuggestionCmd(
  maybeRequestId: Option[Api.RequestId],
  val request: Api.ImportSuggestionRequest
) extends Command(maybeRequestId) {

  import ImportSuggestionCmd._

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    * @param ec execution context
    */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = Future {
    val suggestion = request.suggestion
    reply(
      Api.ImportSuggestionResponse(
        suggestion.module,
        suggestion.name,
        findExports.sortBy(_.depth).map(_.export)
      )
    )
  }

  /** Find re-exports of the given symbol.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  private def findExports(implicit ctx: RuntimeContext): Seq[ExportResult] = {
    val suggestion = request.suggestion
    val topScope =
      ctx.executionService.getContext.getCompiler.context.getTopScope
    val builder = Vector.newBuilder[ExportResult]

    topScope.getModules
      .stream()
      .filter(isCompiled)
      .forEach { module =>
        module.getIr.getMetadata(BindingAnalysis).foreach { bindings =>
          builder ++= getQualifiedExport(
            module,
            suggestion,
            bindings
          )
          builder ++= getUnqualifiedExport(module, suggestion, bindings)
        }
      }

    builder.result()
  }

  /** Extract the qualified export from the bindings map. */
  private def getQualifiedExport(
    module: Module,
    suggestion: Suggestion,
    bindings: BindingsMap
  ): Option[ExportResult] = {
    bindings.resolvedExports
      .find(_.module.getName.toString == suggestion.module)
      .filter(_.exportedAs.isDefined)
      .map { exportedModule =>
        val qualified = Api.Export.Qualified(
          module.getName.toString,
          exportedModule.exportedAs
        )
        ExportResult(qualified, getDepth(module))
      }
  }

  /** Extract the unqualified export from the bindings map. */
  private def getUnqualifiedExport(
    module: Module,
    suggestion: Suggestion,
    bindings: BindingsMap
  ): Option[ExportResult] = {
    bindings.exportedSymbols.get(suggestion.name).flatMap { resolvedExports =>
      resolvedExports
        .find(_.module.getName.toString == suggestion.module)
        .map { _ =>
          val unqualified = Api.Export.Unqualified(module.getName.toString)
          ExportResult(unqualified, getDepth(module))
        }
    }
  }

  private def getDepth(module: Module): Int =
    module.getName.path.size

  private def isCompiled(module: Module): Boolean =
    module.getIr != null
}

object ImportSuggestionCmd {

  /** Module that exports target symbol.
    *
    * @param name the module name
    */
  private case class ExportingModule(name: String)

  /** An intermediate result of exports resolution.
    *
    * @param export the module export
    * @param depth how nested is the exporting module
    */
  private case class ExportResult(export: Api.Export, depth: Int)

}

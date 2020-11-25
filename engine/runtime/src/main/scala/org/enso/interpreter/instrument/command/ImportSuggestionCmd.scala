package org.enso.interpreter.instrument.command

import org.enso.compiler.core.IR
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.runtime.Runtime.Api

import scala.collection.mutable
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
    @scala.annotation.tailrec
    def go(
      queue: mutable.Queue[ExportingModule],
      builder: mutable.Builder[ExportResult, Vector[ExportResult]]
    ): Vector[ExportResult] =
      if (queue.isEmpty) {
        builder.result()
      } else {
        val current = queue.dequeue()
        topScope.getModules
          .stream()
          .filter(isCompiled)
          .forEach { module =>
            module.getIr.exports.foreach { export =>
              if (export.name.name == current.name) {
                if (!export.isAll) {
                  val qualified = Api.Export.Qualified(
                    module.getName.toString,
                    export.rename.map(_.name)
                  )
                  builder += ExportResult(qualified, getDepth(export.name))
                } else if (exportsSymbol(export, suggestion.name)) {
                  val unqualified =
                    Api.Export.Unqualified(module.getName.toString)
                  builder += ExportResult(unqualified, getDepth(export.name))
                  queue.enqueue(ExportingModule(module.getName.toString))
                }
              }
            }
          }
        go(queue, builder)
      }

    go(
      mutable.Queue(ExportingModule(suggestion.module)),
      Vector.newBuilder
    )
  }

  private def getDepth(name: IR.Name.Qualified): Int =
    name.parts.size

  private def isCompiled(module: Module): Boolean =
    module.getIr != null

  private def exportsSymbol(
    export: IR.Module.Scope.Export,
    symbol: String
  ): Boolean = {
    export.isAll &&
    export.onlyNames.forall(_.exists(_.name == symbol))
  }

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

package org.enso.interpreter.instrument.command

import org.enso.compiler.core.IR
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.runtime.Runtime.Api

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A command that gathers info needed for suggestion import.
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
        findExports.sortBy(getDepth)
      )
    )
  }

  private def findExports(implicit ctx: RuntimeContext): Seq[Api.Export] = {
    val suggestion = request.suggestion
    val topScope =
      ctx.executionService.getContext.getCompiler.context.getTopScope
    @scala.annotation.tailrec
    def go(
      queue: mutable.Queue[ExportingModule],
      builder: mutable.Builder[Api.Export, Vector[Api.Export]]
    ): Vector[Api.Export] =
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
                  builder += Api.Export.Qualified(
                    module.getName.toString,
                    export.rename.map(_.name)
                  )
                } else if (exportsSymbol(export, suggestion.name)) {
                  builder += Api.Export.Unqualified(module.getName.toString)
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

  private def getDepth(export: Api.Export): Int =
    export.module.count(_ == '.')

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

}

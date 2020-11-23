package org.enso.interpreter.instrument.command

import org.enso.compiler.core.IR
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A command that gathers info needed for suggestion import.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for suggestion import
  */
class ImportSuggestionCmd(
  maybeRequestId: Option[Api.RequestId],
  val request: Api.ImportSuggestionRequest
) extends Command(maybeRequestId) {

  private case class ModuleExport(
    module: String,
    name: String,
    alias: Option[String]
  )

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
        findExports
      )
    )
  }

  private def findExports(implicit ctx: RuntimeContext): Seq[Api.Export] = {
    @scala.annotation.tailrec
    def go(
      queue: mutable.Queue[ModuleExport],
      builder: mutable.Builder[Api.Export, Vector[Api.Export]]
    ): Vector[Api.Export] =
      if (queue.isEmpty) {
        builder.result()
      } else {
        val elem = queue.dequeue()
        ctx.executionService.getContext.getCompiler.context.getTopScope.getModules
          .forEach { module =>
            module.getIr.exports.foreach { moduleExport =>
              if (
                moduleExport.name.name == elem.module &&
                exportsName(moduleExport, elem.name)
              ) {
                builder += Api.Export(
                  elem.module,
                  moduleExport.rename.map(_.name)
                )
              }
            }
          }
        go(queue, builder)
      }

    val suggestion = request.suggestion
    go(
      mutable.Queue(ModuleExport(suggestion.module, suggestion.name, None)),
      Vector.newBuilder
    )
  }

  private def exportsName(
    export: IR.Module.Scope.Export,
    name: String
  ): Boolean =
    export.onlyNames.forall(_.exists(_.name == name))

}

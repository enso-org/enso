package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.logger.masking.MaskedPath
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.ExecutionContext

/** A command that performs edition of a file.
  *
  * @param request a request for editing
  */
class EditFileCmd(request: Api.EditFileNotification)
    extends SynchronousCommand(None) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit = {
    val logger = ctx.executionService.getLogger
    ctx.locking.withFileLock(
      request.path,
      this.getClass,
      () =>
        ctx.locking.withPendingEditsLock(
          this.getClass,
          () => {
            logger.log(
              Level.FINEST,
              "Adding pending file [{0}] edits [{1}] idMap [{2}]",
              Array[Any](
                MaskedPath(request.path.toPath),
                request.edits.map(e => (e.range, e.text.length)),
                request.idMap.map(_.values.length)
              )
            )
            val edits =
              request.edits.map(edit =>
                PendingEdit.ApplyEdit(edit, request.execute)
              )
            ctx.state.pendingEdits.enqueue(request.path, edits)
            request.idMap.foreach { idMap =>
              ctx.state.pendingEdits.updateIdMap(request.path, idMap)
            }
            if (request.execute) {
              ctx.jobControlPlane.abortAllJobs()
              ctx.jobProcessor
                .run(compileJob())
                .foreach(_ => executeJobs.foreach(ctx.jobProcessor.run))
            } else if (request.idMap.isDefined) {
              ctx.jobProcessor.run(compileJob())
            }
          }
        )
    )
  }

  protected def compileJob(): EnsureCompiledJob = {
    new EnsureCompiledJob(Seq(request.path))
  }

  protected def executeJobs(implicit
    ctx: RuntimeContext
  ): Iterable[ExecuteJob] = {
    ctx.contextManager.getAllContexts
      .collect {
        case (contextId, stack) if stack.nonEmpty =>
          ExecuteJob(contextId, stack.toList)
      }
  }

}

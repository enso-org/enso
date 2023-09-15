package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.{Completion, RuntimeContext}
import org.enso.interpreter.instrument.execution.Completion.{Done, Interrupted}
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import java.util.logging.Level
import scala.concurrent.ExecutionContext

abstract class SynchronousCommand(maybeRequestId: Option[RequestId])
    extends Command(maybeRequestId) {
  type Result[T] = T

  final override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Result[Completion] = {
    val logger = ctx.executionService.getLogger
    logger.log(Level.FINE, s"Executing command synchronously: $this...")
    try {
      executeSynchronously
      Done
    } catch {
      case _: InterruptedException | _: ThreadInterruptedException =>
        Interrupted
      case ex: Throwable =>
        logger.log(
          Level.SEVERE,
          s"An error occurred during execution of $this command",
          ex
        )
        Done
    } finally {
      logger.log(Level.FINE, s"Command $this finished.")
    }
  }

  def executeSynchronously(implicit ctx: RuntimeContext): Unit

}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.{Completion, RuntimeContext}
import org.enso.interpreter.instrument.execution.Completion.{Done, Interrupted}
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import java.util.logging.Level
import scala.concurrent.ExecutionContext

/** `SynchronousCommand`, despite its name,. will still execute asynchronously along with other commands except that
  * the order of execution preserves the order of command's submission (for `SynchronousCommand` kind).
  *
  * The class was added since some commands cannot be executed in arbitrary order but they also must not be
  * executed synchronously due to the possibility of deadlocks.
  * Plain context locks do not necessarily guarantee the right order of such commands.
  */
abstract class SynchronousCommand(maybeRequestId: Option[RequestId])
    extends Command(maybeRequestId) {

  override type Result[T] = T

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
      case ex: Throwable => {
        val msg = s"An error occurred during execution of $this command"
        try {
          logger.log(Level.SEVERE, msg, ex)
        } catch {
          case ise: IllegalStateException =>
            // Thread using TruffleLogger has to have a current context or the TruffleLogger has to be bound to an engine
            ex.printStackTrace()
            ise.initCause(ex)
            throw ise
        }
        Done
      }
    } finally {
      logger.log(Level.FINE, s"Command $this finished.")
    }
  }

  def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit

}

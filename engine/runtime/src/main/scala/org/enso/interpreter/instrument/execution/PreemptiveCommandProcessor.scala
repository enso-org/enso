package org.enso.interpreter.instrument.execution

import java.util.concurrent.{Callable, Executors}
import java.util.logging.Level

import org.enso.interpreter.instrument.command.Command
import org.enso.interpreter.instrument.execution.CommandProcessor.Done
import org.enso.interpreter.runtime.Context

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
  * This component schedules the execution of commands. It keep a queue of
  * pending commands. It activates command execution in FIFO order.
  *
  * @param parallelism the size of the underlying compute thread pool
  * @param context the language context
  */
class PreemptiveCommandProcessor(parallelism: Int, context: Context)
    extends CommandProcessor {

  private val executor = Executors.newFixedThreadPool(
    parallelism,
    new TruffleThreadFactory(context, "truffle-execution-engine")
  )

  /** @inheritdoc **/
  def invoke(cmd: Command, ctx: RuntimeContext): Future[Done.type] = {
    val promise = Promise[Done.type]()
    executor.submit[Unit](new Callable[Unit] {
      override def call(): Unit = {
        val logger = ctx.executionService.getLogger
        logger.log(Level.FINE, s"Executing command: $cmd...")
        try {
          cmd.execute(ctx)
          logger.log(Level.FINE, s"Command $cmd finished.")
          promise.success(Done)
        } catch {
          case NonFatal(ex) => promise.failure(ex)
        }
      }
    })

    promise.future
  }

}

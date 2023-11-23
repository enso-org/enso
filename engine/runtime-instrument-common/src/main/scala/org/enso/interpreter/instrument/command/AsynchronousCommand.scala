package org.enso.interpreter.instrument.command

import com.oracle.truffle.api.TruffleLogger
import org.enso.interpreter.instrument.execution.Completion.{Done, Interrupted}
import org.enso.interpreter.instrument.execution.{Completion, RuntimeContext}
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

abstract class AsynchronousCommand(maybeRequestId: Option[RequestId])
    extends Command(maybeRequestId) {

  override type Result[T] = Future[T]

  final override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Result[Completion] = {
    val logger = ctx.executionService.getLogger
    for {
      _ <- Future {
        logger.log(Level.FINE, s"Executing command asynchronously: $this...")
      }
      result <- mapFailures(logger, executeAsynchronously(ctx, ec))
      _      <- Future { logger.log(Level.FINE, s"Command $this finished.") }
    } yield result
  }

  private def mapFailures(logger: TruffleLogger, result: Future[Unit])(implicit
    ex: ExecutionContext
  ): Future[Completion] = {
    result.transformWith[Completion] {
      case Success(()) =>
        Future.successful(Done)

      case Failure(_: InterruptedException | _: ThreadInterruptedException) =>
        Future.successful[Completion](Interrupted)

      case Failure(NonFatal(ex)) =>
        logger.log(
          Level.SEVERE,
          s"An error occurred during execution of $this command",
          ex
        )
        Future.failed[Completion](ex)

      case Failure(ex) =>
        logger.log(
          Level.SEVERE,
          s"An error occurred during execution of $this command",
          ex
        )
        Future.failed[Completion](ex)
    }
  }

  def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit]
}

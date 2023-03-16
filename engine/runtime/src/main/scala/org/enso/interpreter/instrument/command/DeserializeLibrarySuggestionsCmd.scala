package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.DeserializeLibrarySuggestionsJob
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that initiates the deserialization of suggestions.
  *
  * @param maybeRequestId an option with request id
  */
class DeserializeLibrarySuggestionsCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.DeserializeLibrarySuggestions
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.jobProcessor.runBackground(
      new DeserializeLibrarySuggestionsJob(request.libraryName)
    )
    Future.successful(())
  }
}

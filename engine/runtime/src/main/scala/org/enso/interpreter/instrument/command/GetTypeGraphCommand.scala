package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.`type`.Types
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

class GetTypeGraphCommand(maybeRequestId: Option[RequestId])
    extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = Future {
    val typeGraph = Types.getTypeHierarchy
    reply(Api.GetTypeGraphResponse(typeGraph))
  }
}

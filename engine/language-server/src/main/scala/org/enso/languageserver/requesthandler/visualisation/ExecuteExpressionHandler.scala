package org.enso.languageserver.requesthandler.visualisation

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc._
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.VisualisationApi.ExecuteExpression
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `executionContext/executeExpression` command.
  *
  * @param clientId an unique identifier of the client
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  */
class ExecuteExpressionHandler(
  clientId: ClientId,
  timeout: FiniteDuration,
  contextRegistry: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          ExecuteExpression,
          id,
          params: ExecuteExpression.Params
        ) =>
      contextRegistry ! ContextRegistryProtocol.ExecuteExpression(
        clientId,
        params.visualisationId,
        params.expressionId,
        params.visualisationConfig
      )
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      log.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case ContextRegistryProtocol.VisualisationAttached =>
      replyTo ! ResponseResult(ExecuteExpression, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }

}

object ExecuteExpressionHandler {

  /** Creates configuration object used to create an
    * [[ExecuteExpressionHandler]].
    *
    * @param clientId an unique identifier of the client
    * @param timeout request timeout
    * @param runtime a reference to the context registry
    */
  def props(
    clientId: ClientId,
    timeout: FiniteDuration,
    runtime: ActorRef
  ): Props =
    Props(new ExecuteExpressionHandler(clientId, timeout, runtime))

}

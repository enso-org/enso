package org.enso.languageserver.requesthandler.visualization

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.VisualizationApi.ModifyVisualization
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `executionContext/modifyVisualization` commands.
  *
  * @param clientId an unique identifier of the client
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  */
class ModifyVisualizationHandler(
  clientId: ClientId,
  timeout: FiniteDuration,
  contextRegistry: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ModifyVisualization, id, params: ModifyVisualization.Params) =>
      contextRegistry ! ContextRegistryProtocol.ModifyVisualization(
        clientId,
        params.visualizationId,
        params.visualizationConfig
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
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case ContextRegistryProtocol.VisualizationModified =>
      replyTo ! ResponseResult(ModifyVisualization, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }

}

object ModifyVisualizationHandler {

  /** Creates configuration object used to create a [[ModifyVisualizationHandler]].
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
    Props(new ModifyVisualizationHandler(clientId, timeout, runtime))

}

package org.enso.languageserver.requesthandler.visualisation

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.VisualisationApi.ModifyVisualisation
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `executionContext/modifyVisualisation` commands.
  *
  * @param clientId an unique identifier of the client
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  */
class ModifyVisualisationHandler(
  clientId: ClientId,
  timeout: FiniteDuration,
  contextRegistry: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ModifyVisualisation, id, params: ModifyVisualisation.Params) =>
      contextRegistry ! ContextRegistryProtocol.ModifyVisualisation(
        clientId,
        params.visualisationId,
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
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case ContextRegistryProtocol.VisualisationModified =>
      replyTo ! ResponseResult(ModifyVisualisation, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }

}

object ModifyVisualisationHandler {

  /**
    * Creates configuration object used to create a [[ModifyVisualisationHandler]].
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
    Props(new ModifyVisualisationHandler(clientId, timeout, runtime))

}

package org.enso.languageserver.runtime.handler

import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.Config
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration.FiniteDuration

/** A request handler for modify visualisation commands.
  *
  * @param config the language server config
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
class ModifyVisualisationHandler(
  config: Config,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = { case msg: Api.ModifyVisualisation =>
    runtime ! Api.Request(UUID.randomUUID(), msg)
    val cancellable =
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
    context.become(responseStage(sender(), cancellable))
  }

  private def responseStage(
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      replyTo ! RequestTimeout
      context.stop(self)

    case Api.Response(_, Api.VisualisationModified()) =>
      replyTo ! ContextRegistryProtocol.VisualisationModified
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      replyTo ! RuntimeFailureMapper(config).mapApiError(error)
      cancellable.cancel()
      context.stop(self)
  }

}

object ModifyVisualisationHandler {

  /** Creates configuration object used to create a [[ModifyVisualisationHandler]].
    *
    * @param config the language server config
    * @param timeout request timeout
    * @param runtime reference to the runtime connector
    */
  def props(config: Config, timeout: FiniteDuration, runtime: ActorRef): Props =
    Props(new ModifyVisualisationHandler(config, timeout, runtime))

}

package org.enso.languageserver.runtime.handler

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/** A request handler for push commands.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param timeout request timeout
  * @param runtime reference to the runtime conector
  */
final class PushContextHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = { case msg: Api.PushContextRequest =>
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

    case Api.Response(_, Api.PushContextResponse(contextId)) =>
      replyTo ! ContextRegistryProtocol.PushContextResponse(contextId)
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
      cancellable.cancel()
      context.stop(self)
  }
}

object PushContextHandler {

  /** Creates a configuration object used to create [[PushContextHandler]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param timeout request timeout
    * @param runtime reference to the runtime conector
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    timeout: FiniteDuration,
    runtime: ActorRef
  ): Props =
    Props(new PushContextHandler(runtimeFailureMapper, timeout, runtime))
}

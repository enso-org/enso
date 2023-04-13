package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `executionContext/setExecutionEnvironment` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param session an object representing a client connected to the language server
  */
class SetExecutionEnvironmentHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  session: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import ContextRegistryProtocol._
  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          ExecutionContextSetExecutionEnvironment,
          id,
          params: ExecutionContextSetExecutionEnvironment.Params
        ) =>
      contextRegistry ! SetExecutionEnvironmentRequest(
        session,
        params.contextId,
        params.executionEnvironment
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

    case SetExecutionEnvironmentResponse(_) =>
      replyTo ! ResponseResult(ExecutionContextRecompute, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }
}

object SetExecutionEnvironmentHandler {

  /** Creates configuration object used to create a [[SetExecutionEnvironmentHandler]].
    *
    * @param timeout request timeout
    * @param contextRegistry a reference to the context registry.
    * @param rpcSession an object representing a client connected to the language server
    */
  def props(
    timeout: FiniteDuration,
    contextRegistry: ActorRef,
    rpcSession: JsonSession
  ): Props =
    Props(
      new SetExecutionEnvironmentHandler(timeout, contextRegistry, rpcSession)
    )

}

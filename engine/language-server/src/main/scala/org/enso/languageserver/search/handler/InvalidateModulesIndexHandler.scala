package org.enso.languageserver.search.handler

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.RuntimeFailureMapper
import org.enso.languageserver.search.SearchProtocol
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.duration._

/** A request handler for invalidate modules index command.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param runtime reference to the runtime connector
  * @param suggestionsHandler reference to the suggestions handler
  * @param timeout soft request timeout for detecting abnormal behavior
  */
final class InvalidateModulesIndexHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  runtime: ActorRef,
  suggestionsHandler: ActorRef,
  timeout: FiniteDuration
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case SearchProtocol.InvalidateModulesIndex =>
      val request = Api.Request(
        UUID.randomUUID(),
        Api.InvalidateModulesIndexRequest()
      )
      runtime ! request
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(sender(), request, cancellable, 10))
  }

  private def responseStage(
    replyTo: ActorRef,
    request: Api.Request,
    cancellable: Cancellable,
    retries: Int
  ): Receive = {
    case RequestTimeout =>
      logger.warn(
        "Failed to receive a [{}] response in [{}]. Retrying.",
        request,
        timeout
      )
      val newCancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(
        responseStage(replyTo, request, newCancellable, retries - 1)
      )

    case Api.Response(_, Api.InvalidateModulesIndexResponse()) =>
      suggestionsHandler ! SearchProtocol.ClearSuggestionsDatabase
      replyTo ! SearchProtocol.InvalidateSuggestionsDatabaseResult
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
      cancellable.cancel()
      context.stop(self)
  }
}

object InvalidateModulesIndexHandler {

  /** Creates a configuration object used to create [[InvalidateModulesIndexHandler]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param runtime reference to the runtime connector
    * @param suggestionsHandler reference to the suggestions handler
    * @param timeout soft request timeout for detecting abnormal behavior
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    runtime: ActorRef,
    suggestionsHandler: ActorRef,
    timeout: FiniteDuration = 30.seconds
  ): Props =
    Props(
      new InvalidateModulesIndexHandler(
        runtimeFailureMapper,
        runtime,
        suggestionsHandler,
        timeout
      )
    )
}

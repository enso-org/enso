package org.enso.languageserver.search.handler

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.RuntimeFailureMapper
import org.enso.languageserver.search.SearchProtocol
import org.enso.languageserver.util.{ApiHandlerWithRetries, UnhandledLogging}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.ExecutionContext
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
) extends ApiHandlerWithRetries[
      SearchProtocol.InvalidateModulesIndex.type,
      Api.InvalidateModulesIndexResponse
    ](runtime, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: SearchProtocol.InvalidateModulesIndex.type
  ): Api.Request = {
    Api.Request(
      UUID.randomUUID(),
      Api.InvalidateModulesIndexRequest()
    )
  }

  override protected def positiveResponse(
    replyTo: ActorRef,
    msg: Api.InvalidateModulesIndexResponse
  ): Unit = {
    suggestionsHandler ! SearchProtocol.ClearSuggestionsDatabase
    replyTo ! SearchProtocol.InvalidateSuggestionsDatabaseResult
  }

  override protected def negativeResponse(replyTo: ActorRef, error: Api.Error)(
    implicit ec: ExecutionContext
  ): Unit = {
    runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
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

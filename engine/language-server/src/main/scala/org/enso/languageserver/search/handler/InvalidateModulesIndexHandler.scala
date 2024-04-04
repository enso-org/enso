package org.enso.languageserver.search.handler

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.RuntimeFailureMapper
import org.enso.languageserver.search.SearchProtocol
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

/** A request handler for invalidate modules index command.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param runtime reference to the runtime connector
  * @param suggestionsHandler reference to the suggestions handler
  */
final class InvalidateModulesIndexHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  runtime: ActorRef,
  suggestionsHandler: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case SearchProtocol.InvalidateModulesIndex =>
      runtime ! Api.Request(
        UUID.randomUUID(),
        Api.InvalidateModulesIndexRequest()
      )
      context.become(responseStage(sender()))
  }

  private def responseStage(replyTo: ActorRef): Receive = {
    case Api.Response(_, Api.InvalidateModulesIndexResponse()) =>
      suggestionsHandler ! SearchProtocol.ClearSuggestionsDatabase
      replyTo ! SearchProtocol.InvalidateSuggestionsDatabaseResult
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
      context.stop(self)
  }
}

object InvalidateModulesIndexHandler {

  /** Creates a configuration object used to create [[InvalidateModulesIndexHandler]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param runtime reference to the runtime connector
    * @param suggestionsHandler reference to the suggestions handler
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    runtime: ActorRef,
    suggestionsHandler: ActorRef
  ): Props =
    Props(
      new InvalidateModulesIndexHandler(
        runtimeFailureMapper,
        runtime,
        suggestionsHandler
      )
    )
}

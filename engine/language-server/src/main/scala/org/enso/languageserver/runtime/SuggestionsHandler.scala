package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.runtime.SearchApi.GetSuggestionsDatabase
import org.enso.languageserver.util.UnhandledLogging
import org.enso.searcher.sql.SqlSuggestionsRepo

import scala.annotation.unused

/**
  * Event listener listens event stream for the suggestion database
  * notifications from the runtime and sends updates to the client. The listener
  * is a singleton and created per context registry.
  *
  * @param sessionRouter the session router
  */
final class SuggestionsHandler(
  @unused sessionRouter: ActorRef,
  @unused repo: SqlSuggestionsRepo
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case GetSuggestionsDatabase =>
      ???
  }
}

object SuggestionsHandler {

  /**
    * Creates a configuration object used to create a [[SuggestionsHandler]].
    *
    * @param sessionRouter the session router
    */
  def props(sessionRouter: ActorRef, repo: SqlSuggestionsRepo): Props =
    Props(new SuggestionsHandler(sessionRouter, repo))

}

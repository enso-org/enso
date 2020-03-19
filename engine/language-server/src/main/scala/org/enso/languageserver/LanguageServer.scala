package org.enso.languageserver

import akka.actor.{Actor, ActorLogging, Stash}
import org.enso.languageserver.data._
import org.enso.languageserver.event.{
  ClientConnected,
  ClientDisconnected,
  ClientEvent
}

object LanguageProtocol {

  /** Initializes the Language Server. */
  case object Initialize

}

/**
  * An actor representing an instance of the Language Server.
  *
  * @param config the configuration used by this Language Server.
  */
class LanguageServer(config: Config)
    extends Actor
    with Stash
    with ActorLogging {
  import LanguageProtocol._

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ClientEvent]): Unit
  }

  override def receive: Receive = {
    case Initialize =>
      log.debug("Language Server initialized.")
      unstashAll()
      context.become(initialized(config))
    case _ => stash()
  }

  def initialized(
    config: Config,
    env: Environment = Environment.empty
  ): Receive = {
    case ClientConnected(client) =>
      log.info("Client connected [{}].", client.id)
      context.become(
        initialized(config, env.addClient(client))
      )

    case ClientDisconnected(clientId) =>
      log.info("Client disconnected [{}].", clientId)
      context.become(initialized(config, env.removeClient(clientId)))
  }
}

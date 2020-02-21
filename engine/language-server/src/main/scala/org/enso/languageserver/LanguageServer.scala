package org.enso.languageserver
import java.io.File
import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}

object LanguageProtocol {
  type ClientId = UUID
  case class Initialize(config: Config)
  case class Connect(id: ClientId, client: Client)
  case class Disconnect(clientId: ClientId)

  case class Config(contentRoots: List[File], languagePath: List[File])

  case class Capabilities(hasWriteLock: Boolean) {
    def acquireWriteLock: Capabilities = copy(hasWriteLock = true)
    def releaseWriteLock: Capabilities = copy(hasWriteLock = false)
  }

  object Capabilities {
    val default: Capabilities = Capabilities(false)
  }

  case class Client(actor: ActorRef, capabilities: Capabilities) {
    def acquireWriteLock: Client =
      copy(capabilities = capabilities.acquireWriteLock)
    def releaseWriteLock: Client =
      copy(capabilities = capabilities.releaseWriteLock)
    def hasWriteLock: Boolean = capabilities.hasWriteLock
  }

  case class Env(clients: Map[ClientId, Client]) {
    def addClient(id: ClientId, client: Client): Env = {
      copy(clients = clients + (id -> client))
    }

    def removeClient(clientId: ClientId): Env =
      copy(clients = clients - clientId)

    def mapClients(fun: (ClientId, Client) => Client): Env =
      copy(clients = clients.map { case (id, client) => (id, fun(id, client)) })
  }

  object Env {
    def empty: Env = Env(Map())
  }

  case object ForceReleaseWriteLock
  case class AcquireWriteLock(clientId: ClientId)
  case object WriteLockAcquired
}

class Server extends Actor with Stash with ActorLogging {
  import LanguageProtocol._

  override def receive: Receive = {
    case Initialize(config) =>
      log.debug("Language Server initialized.")
      unstashAll()
      context.become(initialized(config))
    case _ => stash()
  }

  def initialized(config: Config, env: Env = Env.empty): Receive = {
    case Connect(clientId, client) =>
      log.debug("Client connected [{}].", clientId)
      context.become(initialized(config, env.addClient(clientId, client)))
    case Disconnect(clientId) =>
      log.debug("Client disconnected [{}].", clientId)
      context.become(initialized(config, env.removeClient(clientId)))
    case AcquireWriteLock(clientId) =>
      val newEnv = env.mapClients {
        case (id, client) =>
          if (id == clientId) {
            log.debug("Client {} has acquired the write lock.", clientId)
            sender ! WriteLockAcquired
            client.acquireWriteLock
          } else if (client.hasWriteLock) {
            log.debug("Client {} has lost the write lock.", clientId)
            client.actor ! ForceReleaseWriteLock
            client.releaseWriteLock
          } else client
      }
      context.become(initialized(config, newEnv))
  }
}

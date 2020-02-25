package org.enso.languageserver

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import cats.effect.IO
import org.enso.languageserver.data._
import org.enso.languageserver.filemanager.FileManagerProtocol.{
  FileWrite,
  FileWriteResult
}
import org.enso.languageserver.filemanager.{FileSystemApi, FileSystemFailure}

object LanguageProtocol {

  /** Initializes the Language Server. */
  case object Initialize

  /**
    * Notifies the Language Server about a new client connecting.
    *
    * @param clientId the internal client id.
    * @param clientActor the actor this client is represented by.
    */
  case class Connect(clientId: Client.Id, clientActor: ActorRef)

  /**
    * Notifies the Language Server about a client disconnecting.
    * The client may not send any further messages after this one.
    *
    * @param clientId the id of the disconnecting client.
    */
  case class Disconnect(clientId: Client.Id)

  /**
    * Requests the Language Server grant a new capability to a client.
    *
    * @param clientId the client to grant the capability to.
    * @param registration the capability to grant.
    */
  case class AcquireCapability(
    clientId: Client.Id,
    registration: CapabilityRegistration
  )

  /**
    * Notifies the Language Server about a client releasing a capability.
    *
    * @param clientId the client releasing the capability.
    * @param capabilityId the capability being released.
    */
  case class ReleaseCapability(
    clientId: Client.Id,
    capabilityId: CapabilityRegistration.Id
  )

  /**
    * A notification sent by the Language Server, notifying a client about
    * a capability being taken away from them.
    *
    * @param capabilityId the capability being released.
    */
  case class CapabilityForceReleased(capabilityId: CapabilityRegistration.Id)

  /**
    * A notification sent by the Language Server, notifying a client about a new
    * capability being granted to them.
    *
    * @param registration the capability being granted.
    */
  case class CapabilityGranted(registration: CapabilityRegistration)
}

/**
  * An actor representing an instance of the Language Server.
  *
  * @param config the configuration used by this Language Server.
  */
class LanguageServer(config: Config, fs: FileSystemApi[IO])
    extends Actor
    with Stash
    with ActorLogging {
  import LanguageProtocol._

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
    case Connect(clientId, actor) =>
      log.debug("Client connected [{}].", clientId)
      context.become(
        initialized(config, env.addClient(Client(clientId, actor)))
      )

    case Disconnect(clientId) =>
      log.debug("Client disconnected [{}].", clientId)
      context.become(initialized(config, env.removeClient(clientId)))

    case AcquireCapability(
        clientId,
        reg @ CapabilityRegistration(_, capability: CanEdit)
        ) =>
      val (envWithoutCapability, releasingClients) = env.removeCapabilitiesBy {
        case CapabilityRegistration(_, CanEdit(file)) => file == capability.path
        case _                                        => false
      }
      releasingClients.foreach {
        case (client: Client, capabilities) =>
          capabilities.foreach { registration =>
            client.actor ! CapabilityForceReleased(registration.id)
          }
      }
      val newEnv = envWithoutCapability.grantCapability(clientId, reg)
      context.become(initialized(config, newEnv))

    case ReleaseCapability(clientId, capabilityId) =>
      context.become(
        initialized(config, env.releaseCapability(clientId, capabilityId))
      )

    case FileWrite(path, content) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.write(path.toFile(rootPath), content).unsafeRunSync()
        } yield ()

      sender ! FileWriteResult(result)
  }
}

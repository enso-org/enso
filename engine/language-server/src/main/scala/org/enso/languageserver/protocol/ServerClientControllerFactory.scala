package org.enso.languageserver.protocol

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import org.enso.jsonrpc.ClientControllerFactory

/**
  * Language server client controller factory.
  *
  * @param server the language server actor ref
  * @param bufferRegistry the buffer registry actor ref
  * @param capabilityRouter the capability router actor ref
  * @param system the actor system
  */
class ServerClientControllerFactory(
  server: ActorRef,
  bufferRegistry: ActorRef,
  capabilityRouter: ActorRef
)(implicit system: ActorSystem)
    extends ClientControllerFactory {

  /**
    * Creates a client controller actor.
    *
    * @param clientId the internal client id.
    * @return
    */
  override def createClientController(clientId: UUID): ActorRef =
    system.actorOf(
      ClientController.props(clientId, server, bufferRegistry, capabilityRouter)
    )
}

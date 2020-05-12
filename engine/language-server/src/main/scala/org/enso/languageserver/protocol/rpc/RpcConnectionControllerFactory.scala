package org.enso.languageserver.protocol.rpc

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import org.enso.jsonrpc.ClientControllerFactory

/**
  * Language server client controller factory.
  *
  * @param bufferRegistry the buffer registry actor ref
  * @param capabilityRouter the capability router actor ref
  * @param system the actor system
  */
class RpcConnectionControllerFactory(
  bufferRegistry: ActorRef,
  capabilityRouter: ActorRef,
  fileManager: ActorRef,
  contextRegistry: ActorRef
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
      RpcConnectionController.props(
        clientId,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        contextRegistry
      )
    )
}

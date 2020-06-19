package org.enso.languageserver.protocol.json

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
class JsonConnectionControllerFactory(
  bufferRegistry: ActorRef,
  capabilityRouter: ActorRef,
  fileManager: ActorRef,
  contextRegistry: ActorRef,
  stdOutController: ActorRef,
  stdErrController: ActorRef,
  stdInController: ActorRef,
  runtimeConnector: ActorRef
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
      JsonConnectionController.props(
        clientId,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        contextRegistry,
        stdOutController,
        stdErrController,
        stdInController,
        runtimeConnector
      )
    )
}

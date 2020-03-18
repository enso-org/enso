package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import org.enso.jsonrpc.ClientControllerFactory

/**
  * Project manager client controller factory.
  *
  * @param system the actor system
  */
class ManagerClientControllerFactory(system: ActorSystem)
    extends ClientControllerFactory {

  /**
    * Creates a client controller actor.
    *
    * @param clientId the internal client id.
    * @return an actor ref to the client controller
    */
  override def createClientController(clientId: UUID): ActorRef =
    system.actorOf(ClientController.props(clientId))

}

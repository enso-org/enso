package org.enso.projectmanager.protocol

import java.util.UUID

import akka.actor.{Actor, Props}

/**
  * An actor handling communications between a single client and the project
  * manager.
  *
  * @param clientId the internal client id.
  */
class ClientController(clientId: UUID) extends Actor {
  override def receive: Receive = ???
}

object ClientController {

  /**
    * Creates a configuration object used to create a [[ClientController]].
    *
    * @param clientId the internal client id.
    * @return a configuration object
    */
  def props(clientId: UUID): Props = Props(new ClientController(clientId))

}

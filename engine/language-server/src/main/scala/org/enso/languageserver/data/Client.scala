package org.enso.languageserver.data
import java.util.UUID

import akka.actor.ActorRef

/**
  * An object representing a client connected to the language server.
  * @param id the internal id of this client
  * @param actor the actor handling remote client communications, used to push
  *              requests and notifications.
  * @param capabilities the capabilities this client has available.
  */
case class Client(
  id: Client.Id,
  actor: ActorRef,
  capabilities: List[CapabilityRegistration]
)

object Client {
  type Id = UUID

  def apply(id: Id, actor: ActorRef): Client = Client(id, actor, List())
}

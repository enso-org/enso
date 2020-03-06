package org.enso.languageserver.data
import java.util.UUID

import akka.actor.ActorRef

/**
  * An object representing a client connected to the language server.
  * @param id the internal id of this client
  * @param actor the actor handling remote client communications, used to push
  *              requests and notifications.
  */
case class Client(
  id: Client.Id,
  actor: ActorRef
)

object Client {

  type Id = UUID

}

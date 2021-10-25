package org.enso.languageserver.session

import akka.actor.ActorRef
import org.enso.languageserver.data.ClientId

/** An object representing a client connected to the language server via data
  * protocol.
  *
  * @param clientId the internal id of this client
  * @param dataController the actor handling remote client communications, used
  *                      to push requests and notifications.
  */
case class BinarySession(clientId: ClientId, dataController: ActorRef)

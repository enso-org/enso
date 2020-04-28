package org.enso.languageserver.session

import akka.actor.ActorRef
import org.enso.languageserver.data.ClientId

/**
  * An object representing a client connected to the language server via RPC
  * protocol.
  *
  * @param clientId the internal id of this client
  * @param rpcController the actor handling remote client communications, used
  *                      to push requests and notifications.
  */
case class RpcSession(
  clientId: ClientId,
  rpcController: ActorRef
)

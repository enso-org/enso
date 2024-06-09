package org.enso.jsonrpc

import java.util.UUID

import akka.actor.ActorRef

/** Classes implementing this trait are responsible for creating client
  * controllers upon a new connection. An client controller handles
  * communications between a single client and the JSON RPC server.
  */
trait ClientControllerFactory {

  /** Creates a client controller actor.
    *
    * @param clientId the internal client id.
    * @return an actor ref to the client controller
    */
  def createClientController(clientId: UUID): ActorRef

  /** Shutdown resources neccessary for creating client controller actor.
    */
  def shutdown(): Unit

}

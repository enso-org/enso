package org.enso.languageserver.runtime

import akka.actor.ActorRef

object ContextRegistryProtocol {

  /**
    * A request to the context registry to create a new execution context.
    *
    * @param client reference to the client
    */
  case class CreateContextRequest(client: ActorRef)

}

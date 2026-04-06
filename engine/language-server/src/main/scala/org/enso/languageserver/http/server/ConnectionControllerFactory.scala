package org.enso.languageserver.http.server

import akka.actor.ActorRef

/** A factory of connection controllers. */
trait ConnectionControllerFactory {

  /** Creates a connection controller that acts as front controller.
    *
    * @return actor ref of created connection controller
    */
  def createController(): ActorRef
}

package org.enso.languageserver.protocol.binary

import akka.actor.{ActorRef, ActorSystem, Props}
import org.enso.languageserver.http.server.ConnectionControllerFactory

/** A factory for binary connection controllers.
  *
  * @param system an actor system that hosts created connection controllers
  */
class BinaryConnectionControllerFactory(fileManager: ActorRef)(implicit
  system: ActorSystem
) extends ConnectionControllerFactory {

  /** @inheritdoc */
  override def createController(): ActorRef = {
    system.actorOf(Props(new BinaryConnectionController(fileManager)))
  }

}

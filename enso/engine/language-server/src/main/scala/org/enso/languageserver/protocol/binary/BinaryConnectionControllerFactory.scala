package org.enso.languageserver.protocol.binary

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.RemoteAddress
import org.enso.languageserver.http.server.ConnectionControllerFactory

/** A factory for binary connection controllers.
  *
  * @param system an actor system that hosts created connection controllers
  */
class BinaryConnectionControllerFactory(fileManager: ActorRef)(implicit
  system: ActorSystem
) extends ConnectionControllerFactory {

  /** @inheritdoc */
  override def createController(clientIp: RemoteAddress.IP): ActorRef = {
    system.actorOf(Props(new BinaryConnectionController(clientIp, fileManager)))
  }

}

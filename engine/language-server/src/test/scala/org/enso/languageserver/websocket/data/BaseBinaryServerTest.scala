package org.enso.languageserver.websocket.data
import akka.actor.{ActorRef, Props}
import akka.http.scaladsl.model.RemoteAddress
import org.enso.languageserver.http.server.ConnectionControllerFactory
import org.enso.languageserver.protocol.data.BinaryConnectionController

class BaseBinaryServerTest extends BinaryServerTestKit {

  @volatile
  protected var lastConnectionController: ActorRef = _

  override def connectionControllerFactory: ConnectionControllerFactory = {
    (clientIp: RemoteAddress.IP) =>
      {
        val controller =
          system.actorOf(Props(new BinaryConnectionController(clientIp)))
        lastConnectionController = controller
        controller
      }
  }

}

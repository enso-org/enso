package org.enso.projectmanager.test

import java.net.{InetSocketAddress, Socket => JSocket}

import cats.implicits._
import org.enso.projectmanager.data.Socket

object Net {

  def tryConnect(socket: Socket): Either[Throwable, Unit] =
    tryConnect(new InetSocketAddress(socket.host, socket.port), 500)

  def tryConnect(
    inetSocketAddress: InetSocketAddress,
    timeout: Int
  ): Either[Throwable, Unit] =
    Either.catchNonFatal {
      val socket = new JSocket()
      socket.connect(inetSocketAddress, timeout)
      socket.getChannel
      socket.close()
    }

}

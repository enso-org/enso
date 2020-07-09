package org.enso.projectmanager.infrastructure.net
import java.net.InetAddress

import javax.net.ServerSocketFactory

import scala.annotation.tailrec
import scala.util.Random

/**
  * A namespace for TCP auxiliary functions.
  */
object Tcp {

  /**
    * Finds first available socket.
    *
    * @param host a host
    * @param minPort a minimum value of port
    * @param maxPort a maximum value of port
    * @return a port that is available to bind
    */
  @tailrec
  def findAvailablePort(host: String, minPort: Int, maxPort: Int): Int = {
    val random = Random.nextInt(maxPort - minPort + 1)
    val port   = minPort + random
    if (isPortAvailable(host, port)) {
      port
    } else {
      findAvailablePort(host, minPort, maxPort)
    }
  }

  /**
    * Checks if socket is available.
    *
    * @param host a host
    * @param port a port
    * @return true if socket is available
    */
  def isPortAvailable(host: String, port: Int): Boolean =
    try {
      val serverSocket = ServerSocketFactory.getDefault.createServerSocket(
        port,
        1,
        InetAddress.getByName(host)
      )
      serverSocket.close()
      true
    } catch {
      case _: Exception => false
    }

}

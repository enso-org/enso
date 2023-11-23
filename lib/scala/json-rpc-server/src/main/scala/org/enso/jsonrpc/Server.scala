package org.enso.jsonrpc

import akka.actor.ActorSystem
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.http.scaladsl.server.Route

import scala.concurrent.Future

abstract class Server(implicit private val system: ActorSystem) {

  /** Binds this server instance to a given port and interface, allowing
    * future connections.
    *
    * @param interface the interface to bind to
    * @param port      the port to bind to
    * @param secure    true if the port should refer to a secure binding
    * @return a server binding object
    */
  def bind(
    interface: String,
    port: Int,
    secure: Boolean = false
  ): Future[Http.ServerBinding] = {
    val httpServer = Http()
      .newServerAt(interface, port)
    if (secure) {
      val httpsContext = secureConfig().flatMap(config => {
        config
          .generateSSLContext()
          .map(ctx => ConnectionContext.httpsServer(ctx))
          .toOption

      })
      httpsContext match {
        case Some(ctx) =>
          httpServer.enableHttps(ctx).bind(serverRoute(port))
        case None =>
          Future.failed(new RuntimeException("HTTPS misconfigured"))
      }
    } else {
      httpServer.bind(serverRoute(port))
    }
  }

  /** Returns handlers for http requests supported by the server.
    *
    * @param port port number where the server will be listening to handle requests
    * @return mapping between requests and responses supported by this server
    */
  protected def serverRoute(port: Int): Route

  /** Returns an optional configuration for supporting secure connections. */
  protected def secureConfig(): Option[SecureConnectionConfig]

}

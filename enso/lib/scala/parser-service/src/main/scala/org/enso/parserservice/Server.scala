package org.enso.parserservice

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.model.ws.UpgradeToWebSocket
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.Success

object Server {

  /** Describes endpoint to which [[Server]] can bind. */
  final case class Config(interface: String, port: Int) {
    def addressString(): String = s"ws://$interface:$port"
  }
}

/** WebSocket server supporting synchronous request-response protocol.
  *
  * Server when run binds to endpoint and accepts establishing web socket
  * connection for any number of peers.
  *
  * Server replies to each incoming text message with a single text message.
  * Server accepts a single Text Message from a peer and responds with
  * another Text Message.
  */
trait Server {
  implicit val system: ActorSystem = ActorSystem()

  /** Generate text reply for given request text message. */
  def handleMessage(input: String): String

  /** Akka stream defining server behavior.
    *
    * Incoming [[TextMessage]]s are replied to (see [[handleMessage]]).
    * Incoming binary messages are ignored.
    */
  val handlerFlow: Flow[Message, TextMessage.Strict, NotUsed] =
    Flow[Message]
      .flatMapConcat {
        case tm: TextMessage =>
          val strict = tm.textStream.fold("")(_ + _)
          strict.map(input => TextMessage(handleMessage(input)))
        case bm: BinaryMessage =>
          bm.dataStream.runWith(Sink.ignore)
          Source.empty
      }

  /** Server behavior upon receiving HTTP request.
    *
    * As server implements websocket-based protocol, this implementation accepts
    * only GET requests to set up WebSocket connection.
    *
    * The request's URI is not checked.
    */
  @nowarn("cat=deprecation")
  val handleRequest: HttpRequest => HttpResponse = {
    case req @ HttpRequest(GET, _, _, _, _) =>
      req.header[UpgradeToWebSocket] match {
        case Some(upgrade) =>
          println("Establishing a new connection")
          upgrade.handleMessages(handlerFlow)
        case None =>
          HttpResponse(
            StatusCodes.BadRequest,
            entity = "Not a valid websocket request!"
          )
      }
    case r: HttpRequest =>
      r.discardEntityBytes()
      HttpResponse(StatusCodes.MethodNotAllowed)
  }

  /** Starts a HTTP server listening at the given endpoint.
    *
    * Function is asynchronous, will return immediately. If the server fails to
    * start, function will exit the process with a non-zero code.
    */
  def start(config: Server.Config): Unit = {
    val bindingFuture =
      Http().bindAndHandleSync(
        handleRequest,
        interface = config.interface,
        port      = config.port
      )

    bindingFuture.onComplete({
      case Success(_) =>
        println(s"Server online at ${config.addressString()}")
      case Failure(exception) =>
        println(s"Failed to start server: $exception")
        system.terminate()
        System.exit(1)
    })(ExecutionContext.global)
  }
}

package org.enso.loggingservice.internal.service

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.ws.{
  BinaryMessage,
  Message,
  TextMessage,
  UpgradeToWebSocket
}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.stream.scaladsl.{Flow, Sink, Source}
import io.circe.{parser, Error}
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogger
}
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.Printer
import org.enso.loggingservice.{LogLevel, ServerBinding}

import scala.annotation.nowarn
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/** A server [[Service]] which handles both messages incoming from a WebSocket
  * connection and local log messages.
  *
  * @param interface interface to bind to
  * @param port port to bind to; if set to 0, the system will allocate some
  *             available port
  * @param queue log message queue
  * @param printers printers defining handling of the log messages
  * @param logLevel log level used to filter the local messages (messages from
  *                 clients are passed as-is as they may use different log
  *                 levels)
  */
class Server(
  interface: String,
  port: Int,
  queue: BlockingConsumerMessageQueue,
  printers: Seq[Printer],
  logLevel: LogLevel
) extends Local(logLevel, queue, printers)
    with ServiceWithActorSystem {

  /** @inheritdoc
    */
  override protected def actorSystemName: String = "logging-service-server"

  /** Immediately starts processing local messages and returns a [[Future]] that
    * will complete once the server has been started.
    */
  def start(): Future[Unit] = {
    startQueueProcessor()
    startWebSocketServer()
  }

  /** Starts the WebSocket server.
    */
  private def startWebSocketServer(): Future[Unit] = {
    val requestHandler: HttpRequest => HttpResponse = {
      case req @ HttpRequest(GET, Uri.Path("/"), _, _, _) =>
        req.header[UpgradeToWebSocket @nowarn] match {
          case Some(upgrade) =>
            val flow = Flow.fromSinkAndSourceCoupled(
              createMessageProcessor(),
              Source.never
            )
            upgrade.handleMessages(flow)
          case None =>
            HttpResponse(400, entity = "Not a valid websocket request!")
        }
      case r: HttpRequest =>
        r.discardEntityBytes()
        HttpResponse(404, entity = "Unknown resource!")
    }

    import actorSystem.dispatcher
    Http()
      .bindAndHandleSync(
        requestHandler,
        interface = interface,
        port      = port
      )
      .map { serverBinding =>
        bindingOption = Some(serverBinding)
      }
  }

  /** Returns the binding that describes how to connect to the started server.
    *
    * This method can only be called after the future returned from [[start]]
    * has completed.
    */
  def getBinding(): ServerBinding = {
    val binding = bindingOption.getOrElse(
      throw new IllegalStateException(
        "Binding requested before the server has been initialized."
      )
    )

    ServerBinding(port = binding.localAddress.getPort)
  }

  private var bindingOption: Option[Http.ServerBinding] = None

  /** Creates a separate message processor for each connection.
    *
    * Each connection will only report the first invalid message, all further
    * invalid messages are silently ignored.
    */
  private def createMessageProcessor() = {
    @volatile var invalidWasReported: Boolean = false
    def reportInvalidMessage(error: Throwable): Unit = {
      if (!invalidWasReported) {
        InternalLogger.error(s"Invalid message: $error.")
        invalidWasReported = true
      }
    }
    Sink.foreach[Message] {
      case tm: TextMessage =>
        val rawMessage     = tm.textStream.fold("")(_ + _)
        val decodedMessage = rawMessage.map(decodeMessage)
        decodedMessage.runForeach {
          case Left(error)    => reportInvalidMessage(error)
          case Right(message) => queue.send(Right(message))
        }
      case bm: BinaryMessage =>
        reportInvalidMessage(
          new IllegalStateException("Unexpected binary message.")
        )
        bm.dataStream.runWith(Sink.ignore)
    }
  }

  private def decodeMessage(message: String): Either[Error, WSLogMessage] =
    parser.parse(message).flatMap(_.as[WSLogMessage])

  /** Shuts down the server.
    */
  override protected def terminateUser(): Future[_] = {
    bindingOption match {
      case Some(binding) =>
        binding.terminate(hardDeadline = 2.seconds)
      case None => Future.successful(())
    }
  }
}

object Server {

  /** Waits for the [[Server]] to start up and returns it or throws an exception
    * on setup failure.
    *
    * @param interface interface to bind to
    * @param port port to bind to; if set to 0, the system will allocate some
    *             available port
    * @param queue log message queue
    * @param printers printers defining handling of the log messages
    * @param logLevel log level used to filter the local messages (messages from
    *                 clients are passed as-is as they may use different log
    *                 levels)
    */
  def setup(
    interface: String,
    port: Int,
    queue: BlockingConsumerMessageQueue,
    printers: Seq[Printer],
    logLevel: LogLevel
  ): Server = {
    val server = new Server(interface, port, queue, printers, logLevel)
    try {
      Await.result(server.start(), 3.seconds)
      server
    } catch {
      case e: Throwable =>
        server.terminate()
        throw e
    }
  }
}

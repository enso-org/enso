package org.enso.loggingservice.internal.serviceconnection

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Sink, Source}
import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage

import scala.concurrent.Future

class Client(
  serverUri: Uri,
  protected val queue: BlockingConsumerMessageQueue,
  protected val logLevel: LogLevel
) extends ThreadProcessingService
    with ServiceWithActorSystem {

  override protected def actorSystemName: String = "logging-service-client"

  def start(): Unit = {
    val request  = WebSocketRequest(serverUri)
    val flow     = Http().webSocketClientFlow(request)
    val incoming = Sink.ignore
    val outgoing = Source.queue[TextMessage](0, OverflowStrategy.backpressure)

    val ((outgoingQueue, upgradeResponse), closed) =
      outgoing.viaMat(flow)(Keep.both).toMat(incoming)(Keep.both).run()

    import actorSystem.dispatcher
    val connected = upgradeResponse.flatMap { upgrade =>
      if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
        Future.successful(Done)
      } else {
        throw new RuntimeException(
          s"Connection failed: ${upgrade.response.status}"
        )
      }
    }

    // TODO on connected start queue processor
    // TODO try throwing errors here and make start a Future and wait for it in setup
    // TODO on disconnect, warn and start a fallback?
  }

  override protected def processMessage(message: WSLogMessage): Unit = ???

  override protected def terminateUser(): Future[_] = {
    import actorSystem.dispatcher
    Future(())
  }
}

object Client {
  def setup(
    serverUri: Uri,
    queue: BlockingConsumerMessageQueue,
    logLevel: LogLevel
  ): Client = {
    val client = new Client(serverUri, queue, logLevel)
    try {
      client.start()
      client
    } catch {
      case e: Throwable =>
        client.terminate()
        throw e
    }
  }
}

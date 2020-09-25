package org.enso.loggingservice.internal.serviceconnection

import akka.Done
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{OverflowStrategy, QueueOfferResult}
import io.circe.syntax._
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.{InternalLogger, LogLevel, WSLoggerManager}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class Client(
  serverUri: Uri,
  protected val queue: BlockingConsumerMessageQueue,
  protected val logLevel: LogLevel
) extends ThreadProcessingService
    with ServiceWithActorSystem {

  override protected def actorSystemName: String = "logging-service-client"

  def start(): Future[Unit] = {
    val request  = WebSocketRequest(serverUri)
    val flow     = Http().webSocketClientFlow(request)
    val incoming = Sink.ignore
    val outgoing = Source.queue[Message](0, OverflowStrategy.backpressure)

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

    connected.map(_ => {
      closedConnection = Some(closed)
      webSocketQueue   = Some(outgoingQueue)

      closed.onComplete(_ => onDisconnected())

      startQueueProcessor()
    })
  }

  private var webSocketQueue: Option[SourceQueueWithComplete[Message]] = None
  private var closedConnection: Option[Future[Done]]                   = None

  override protected def processMessage(message: WSLogMessage): Unit = {
    val queue = webSocketQueue.getOrElse(
      throw new IllegalStateException(
        "Internal error: The queue processor thread has been started before " +
        "the connection has been initialized."
      )
    )
    val serializedMessage = message.asJson.noSpaces
    val offerResult       = queue.offer(TextMessage.Strict(serializedMessage))
    try {
      Await.result(offerResult, 30.seconds) match {
        case QueueOfferResult.Enqueued =>
        case QueueOfferResult.Dropped =>
          InternalLogger.error(s"A log message has been dropped unexpectedly.")
        case QueueOfferResult.Failure(cause) =>
          InternalLogger.error(s"A log message could not be sent: $cause.")
        case QueueOfferResult.QueueClosed => throw new InterruptedException
      }
    } catch {
      case _: concurrent.TimeoutException =>
        InternalLogger.error(
          s"Adding a log message timed out. Messages may or may not be dropped."
        )
    }
  }

  @volatile private var shuttingDown: Boolean = false

  private def onDisconnected(): Unit = {
    if (!shuttingDown) {
      InternalLogger.error(
        "Server has disconnected, logging is falling back to stderr."
      )
      WSLoggerManager.replaceWithFallback()
    }
  }

  override protected def terminateUser(): Future[_] = {
    shuttingDown = true
    webSocketQueue match {
      case Some(wsQueue) =>
        import actorSystem.dispatcher
        val promise = concurrent.Promise[Done]()
        wsQueue.complete()
        wsQueue.watchCompletion().onComplete(promise.tryComplete)
        closedConnection.foreach(_.onComplete(promise.tryComplete))
        promise.future
      case None => Future.successful(Done)
    }
  }

  override protected def shutdownProcessors(): Unit = {}
}

object Client {
  def setup(
    serverUri: Uri,
    queue: BlockingConsumerMessageQueue,
    logLevel: LogLevel
  ): Client = {
    val client = new Client(serverUri, queue, logLevel)
    try {
      Await.result(client.start(), 3.seconds)
      client
    } catch {
      case e: Throwable =>
        client.terminate()
        throw e
    }
  }
}

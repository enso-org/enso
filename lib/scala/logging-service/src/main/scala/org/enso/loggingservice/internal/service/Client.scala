package org.enso.loggingservice.internal.service

import akka.Done
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.http.scaladsl.model.{StatusCodes, Uri}
import akka.stream.scaladsl.{Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{OverflowStrategy, QueueOfferResult}
import io.circe.syntax._
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogger
}
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.{LogLevel, LoggingServiceManager}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
  * A client [[Service]] that passes incoming log messages to a server.
  *
  * @param serverUri uri of the server to connect to
  * @param queue log message queue
  * @param logLevel log level used to filter messages
  */
class Client(
  serverUri: Uri,
  protected val queue: BlockingConsumerMessageQueue,
  protected val logLevel: LogLevel
) extends ThreadProcessingService
    with ServiceWithActorSystem {

  /**
    * @inheritdoc
    */
  override protected def actorSystemName: String = "logging-service-client"

  /**
    * Starts the client service by trying to connect to the server.
    *
    * Returns a future that is completed once the connection has been
    * established.
    */
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

  /**
    * Tries to send the log message to the server by appending it to the queue
    * of outgoing messages.
    *
    * It waits for the offer to complete for a long time to handle the case in
    * which the server is unresponsive for a longer time. Any pending messages
    * are just enqueued onto the main [[BlockingConsumerMessageQueue]] and will
    * be sent after this one.
    */
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

  /**
    * If the remote server closes the connection, notifies the logging service
    * to start the fallback logger.
    */
  private def onDisconnected(): Unit = {
    if (!shuttingDown) {
      InternalLogger.error(
        "Server has disconnected, logging is falling back to stderr."
      )
      LoggingServiceManager.replaceWithFallback()
    }
  }

  /**
    * Closes the connection.
    */
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

  /**
    * No additional actions are performed after the thread has been shut down as
    * termination happens in [[terminateUser()]].
    */
  override protected def afterShutdown(): Unit = {}
}

object Client {

  /**
    * Waits for the [[Client]] to start up and returns it or throws an exception
    * on setup failure.
    *
    * @param serverUri uri of the server to connect to
    * @param queue log message queue
    * @param logLevel log level used to filter messages
    */
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

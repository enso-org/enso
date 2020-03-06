package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.data.Client
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc._
import org.enso.languageserver.text.TextApi.{CloseFile, FileNotOpenedError}
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol.{FileClosed, FileNotOpened}

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `text/closeFile` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param client an object representing a client connected to the language server
  */
class CloseFileHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(CloseFile, id, params: CloseFile.Params) =>
      bufferRegistry ! TextProtocol.CloseFile(client.id, params.path)
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Closing file for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case FileClosed =>
      replyTo ! ResponseResult(CloseFile, id, Unused)
      context.stop(self)

    case FileNotOpened =>
      replyTo ! ResponseError(Some(id), FileNotOpenedError)
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}

object CloseFileHandler {

  /**
    * Creates a configuration object used to create a [[CloseFileHandler]]
    *
    * @param bufferRegistry a router that dispatches text editing requests
    * @param requestTimeout a request timeout
    * @param client an object representing a client connected to the language server
    * @return a configuration object
    */
  def props(
    bufferRegistry: ActorRef,
    requestTimeout: FiniteDuration,
    client: Client
  ): Props = Props(new CloseFileHandler(bufferRegistry, requestTimeout, client))

}

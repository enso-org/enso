package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.data.Client
import org.enso.languageserver.text.TextApi._
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol.{ApplyEdit => _, _}

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `text/applyEdit` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param client an object representing a client connected to the language server
  */
class ApplyEditHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ApplyEdit, id, params: ApplyEdit.Params) =>
      bufferRegistry ! TextProtocol.ApplyEdit(client.id, params.edit)
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Applying edit for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case ApplyEditSuccess =>
      replyTo ! ResponseResult(ApplyEdit, id, Unused)
      context.stop(self)

    case TextEditValidationFailed(msg) =>
      replyTo ! ResponseError(Some(id), TextEditValidationError(msg))
      context.stop(self)

    case TextEditInvalidVersion(clientVersion, serverVersion) =>
      replyTo ! ResponseError(
        Some(id),
        InvalidVersionError(clientVersion, serverVersion)
      )
      context.stop(self)

    case WriteDenied =>
      replyTo ! ResponseError(Some(id), WriteDeniedError)
      context.stop(self)

    case FileNotOpened =>
      replyTo ! ResponseError(Some(id), FileNotOpenedError)
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}

object ApplyEditHandler {

  /**
    * Creates a configuration object used to create a [[ApplyEditHandler]]
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
  ): Props = Props(new ApplyEditHandler(bufferRegistry, requestTimeout, client))

}

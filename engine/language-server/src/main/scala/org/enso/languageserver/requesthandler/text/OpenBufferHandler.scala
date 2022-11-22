package org.enso.languageserver.requesthandler.text

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.text.TextApi.OpenBuffer
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol.{
  OpenFileResponse,
  OpenFileResult
}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `text/openFile` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param rpcSession an object representing a client connected to the language server
  */
class OpenBufferHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  rpcSession: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(OpenBuffer, id, params: OpenBuffer.Params) =>
      bufferRegistry ! TextProtocol.OpenBuffer(rpcSession, params.path)
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error(
        "Opening buffer request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case OpenFileResponse(Right(OpenFileResult(buffer, capability))) =>
      replyTo ! ResponseResult(
        OpenBuffer,
        id,
        OpenBuffer
          .Result(
            capability,
            buffer.contents.toString,
            buffer.version.toHexString
          )
      )
      cancellable.cancel()
      context.stop(self)

    case OpenFileResponse(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)
  }
}
object OpenBufferHandler {

  /** Creates a configuration object used to create a [[OpenBufferHandler]]
    *
    * @param bufferRegistry a router that dispatches text editing requests
    * @param requestTimeout a request timeout
    * @param rpcSession an object representing a client connected to the language server
    * @return a configuration object
    */
  def props(
    bufferRegistry: ActorRef,
    requestTimeout: FiniteDuration,
    rpcSession: JsonSession
  ): Props =
    Props(new OpenBufferHandler(bufferRegistry, requestTimeout, rpcSession))

}

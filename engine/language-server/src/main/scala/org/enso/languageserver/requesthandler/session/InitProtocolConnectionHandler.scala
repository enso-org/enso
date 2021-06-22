package org.enso.languageserver.requesthandler.session

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.ContentRootManagerProtocol
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.SessionApi.InitProtocolConnection
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `session/initProtocolConnection` commands.
  *
  * @param contentRootManager a content root manager reference
  * @param timeout a request timeout
  */
class InitProtocolConnectionHandler(
  contentRootManager: ActorRef,
  timeout: FiniteDuration
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(InitProtocolConnection, id, _) =>
      contentRootManager ! ContentRootManagerProtocol.GetContentRoots
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
      logger.error("Getting content roots request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case ContentRootManagerProtocol.GetContentRootsResult(contentRoots) =>
      cancellable.cancel()

      val roots = contentRoots.map(_.toContentRoot).toSet

      replyTo ! ResponseResult(
        InitProtocolConnection,
        id,
        InitProtocolConnection.Result(roots)
      )
      context.stop(self)
  }

}

object InitProtocolConnectionHandler {

  /** Creates a configuration object used to create a [[InitProtocolConnectionHandler]]
    *
    * @param contentRootManager a content root manager reference
    * @param timeout a request timeout
    * @return a configuration object
    */
  def props(contentRootManager: ActorRef, timeout: FiniteDuration): Props =
    Props(new InitProtocolConnectionHandler(contentRootManager, timeout))

}

package org.enso.languageserver.requesthandler.vcs

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{
  Errors,
  Id,
  Request,
  ResponseError,
  ResponseResult,
  Unused
}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.vcsmanager.VcsManagerApi.InitVcs
import org.enso.languageserver.vcsmanager.{VcsFailureMapper, VcsProtocol}

import scala.concurrent.duration.FiniteDuration

class InitVcsHandler(
  requestTimeout: FiniteDuration,
  bufferRegistry: ActorRef,
  rpcSession: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(InitVcs, id, params: InitVcs.Params) =>
      bufferRegistry ! VcsProtocol.InitRepo(rpcSession.clientId, params.root)
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error(
        "Initialize project request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case Status.Failure(ex) =>
      logger.error(
        s"Initialize project request [$id] for [${rpcSession.clientId}] failed with: ${ex.getMessage}.",
        ex
      )
      cancellable.cancel()
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      context.stop(self)

    case VcsProtocol.InitRepoResponse(Right(_)) =>
      replyTo ! ResponseResult(InitVcs, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case VcsProtocol.InitRepoResponse(Left(failure)) =>
      replyTo ! ResponseError(Some(id), VcsFailureMapper.mapFailure(failure))
      cancellable.cancel()
      context.stop(self)
  }
}

object InitVcsHandler {

  def props(
    timeout: FiniteDuration,
    bufferRegistry: ActorRef,
    rpcSession: JsonSession
  ): Props =
    Props(new InitVcsHandler(timeout, bufferRegistry, rpcSession))
}

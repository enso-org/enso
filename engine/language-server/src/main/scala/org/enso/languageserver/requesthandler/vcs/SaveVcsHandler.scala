package org.enso.languageserver.requesthandler.vcs

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{Errors, Id, Request, ResponseError, ResponseResult}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.vcsmanager.VcsManagerApi.SaveVcs
import org.enso.languageserver.vcsmanager.{VcsFailureMapper, VcsProtocol}

import scala.concurrent.duration.FiniteDuration

class SaveVcsHandler(
  requestTimeout: FiniteDuration,
  bufferRegistry: ActorRef,
  rpcSession: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(SaveVcs, id, params: SaveVcs.Params) =>
      bufferRegistry ! VcsProtocol.SaveRepo(
        rpcSession.clientId,
        params.root,
        params.name
      )
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(
        responseStage(id, sender(), cancellable)
      )
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error(
        "Save project request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case VcsProtocol.SaveRepoResponse(Right((name, sha))) =>
      replyTo ! ResponseResult(SaveVcs, id, SaveVcs.Result(name, sha))
      cancellable.cancel()
      context.stop(self)

    case VcsProtocol.SaveRepoResponse(Left(failure)) =>
      replyTo ! ResponseError(Some(id), VcsFailureMapper.mapFailure(failure))
      cancellable.cancel()
      context.stop(self)
  }
}

object SaveVcsHandler {

  def props(
    timeout: FiniteDuration,
    bufferRegistry: ActorRef,
    rpcSession: JsonSession
  ): Props =
    Props(new SaveVcsHandler(timeout, bufferRegistry, rpcSession))
}

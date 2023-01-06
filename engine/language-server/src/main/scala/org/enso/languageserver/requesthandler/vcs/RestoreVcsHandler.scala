package org.enso.languageserver.requesthandler.vcs

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.vcsmanager.VcsManagerApi.RestoreVcs
import org.enso.languageserver.vcsmanager.{VcsFailureMapper, VcsProtocol}

import scala.concurrent.duration.FiniteDuration

class RestoreVcsHandler(
  requestTimeout: FiniteDuration,
  bufferManager: ActorRef,
  rpcSession: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(RestoreVcs, id, params: RestoreVcs.Params) =>
      bufferManager ! VcsProtocol.RestoreRepo(
        rpcSession.clientId,
        params.root,
        params.commitId
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
        "Restore project request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case VcsProtocol.RestoreRepoResponse(Right(paths)) =>
      replyTo ! ResponseResult(RestoreVcs, id, RestoreVcs.Result(paths))
      cancellable.cancel()
      context.stop(self)

    case VcsProtocol.RestoreRepoResponse(Left(failure)) =>
      replyTo ! ResponseError(Some(id), VcsFailureMapper.mapFailure(failure))
      cancellable.cancel()
      context.stop(self)
  }
}

object RestoreVcsHandler {
  def props(
    timeout: FiniteDuration,
    bufferManager: ActorRef,
    rpcSession: JsonSession
  ): Props =
    Props(new RestoreVcsHandler(timeout, bufferManager, rpcSession))
}

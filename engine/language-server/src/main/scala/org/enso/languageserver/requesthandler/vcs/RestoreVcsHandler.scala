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
  vcsManager: ActorRef,
  rpcSession: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(RestoreVcs, id, params: RestoreVcs.Params) =>
      vcsManager ! VcsProtocol.RestoreRepo(
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
        "Initialize project request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case VcsProtocol.RestoreRepoResult(Right(_)) =>
      replyTo ! ResponseResult(RestoreVcs, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case VcsProtocol.RestoreRepoResult(Left(failure)) =>
      replyTo ! ResponseError(Some(id), VcsFailureMapper.mapFailure(failure))
      cancellable.cancel()
      context.stop(self)
  }
}

object RestoreVcsHandler {
  def props(
             timeout: FiniteDuration,
             vcsManager: ActorRef,
             rpcSession: JsonSession
           ): Props =
    Props(new RestoreVcsHandler(timeout, vcsManager, rpcSession))
}

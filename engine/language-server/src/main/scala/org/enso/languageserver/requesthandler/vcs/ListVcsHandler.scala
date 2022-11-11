package org.enso.languageserver.requesthandler.vcs

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.vcsmanager.VcsManagerApi.ListVcs
import org.enso.languageserver.vcsmanager.{VcsFailureMapper, VcsProtocol}

import scala.concurrent.duration.FiniteDuration

class ListVcsHandler(
  requestTimeout: FiniteDuration,
  vcsManager: ActorRef,
  rpcSession: JsonSession
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ListVcs, id, params: ListVcs.Params) =>
      vcsManager ! VcsProtocol.ListRepo(
        rpcSession.clientId,
        params.root,
        params.limit
      )
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
        "List project saves request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case VcsProtocol.ListRepoResponse(Right(saves)) =>
      replyTo ! ResponseResult(
        ListVcs,
        id,
        ListVcs.Result(saves.map { case (commitId, message) =>
          ListVcs.Save(commitId, message)
        })
      )
      cancellable.cancel()
      context.stop(self)

    case VcsProtocol.ListRepoResponse(Left(failure)) =>
      replyTo ! ResponseError(Some(id), VcsFailureMapper.mapFailure(failure))
      cancellable.cancel()
      context.stop(self)
  }
}

object ListVcsHandler {
  def props(
    timeout: FiniteDuration,
    vcsManager: ActorRef,
    rpcSession: JsonSession
  ): Props =
    Props(new ListVcsHandler(timeout, vcsManager, rpcSession))
}

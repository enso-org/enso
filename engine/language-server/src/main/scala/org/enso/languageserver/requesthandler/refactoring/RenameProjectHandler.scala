package org.enso.languageserver.requesthandler.refactoring

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.refactoring.RefactoringApi.RenameProject
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `refactoring/renameProject` commands.
  *
  * @param timeout a request timeout
  * @param runtimeConnector a reference to the runtime connector
  */
class RenameProjectHandler(timeout: FiniteDuration, runtimeConnector: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(RenameProject, id, params: RenameProject.Params) =>
      val payload = Api.RenameProject(params.oldName, params.newName)
      runtimeConnector ! Api.Request(UUID.randomUUID(), payload)
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
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Api.Response(_, Api.ProjectRenamed(name)) =>
      context.system.eventStream.publish(ProjectNameChangedEvent(name))
      replyTo ! ResponseResult(RenameProject, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }

}

object RenameProjectHandler {

  /**
    * Creates configuration object used to create a [[RenameProjectHandler]].
    *
    * @param timeout request timeout
    * @param runtimeConnector reference to the runtime connector
    */
  def props(timeout: FiniteDuration, runtimeConnector: ActorRef): Props =
    Props(new RenameProjectHandler(timeout, runtimeConnector))

}

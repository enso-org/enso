package org.enso.languageserver.requesthandler.refactoring

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.refactoring.RefactoringApi.{
  ProjectRenameFailed,
  RenameProject
}
import org.enso.languageserver.refactoring.{RefactoringApi, RefactoringProtocol}
import org.enso.languageserver.util.{
  RequestToApiHandlerWithRetries,
  UnhandledLogging
}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** A request handler for `refactoring/renameProject` commands.
  *
  * @param timeout a request timeout
  * @param target a reference to the runtime connector
  */
class RenameProjectHandler(timeout: FiniteDuration, target: ActorRef)
    extends RequestToApiHandlerWithRetries[
      Request[RenameProject.type, RenameProject.Params],
      Api.ProjectRenamed,
      Api.Error,
      Api.Request
    ](target, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[RefactoringApi.RenameProject.type, RenameProject.Params]
  ): Api.Request = {
    val payload = Api.RenameProject(
      msg.params.namespace,
      msg.params.oldName,
      msg.params.newName
    )
    Api.Request(UUID.randomUUID(), payload)
  }

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      RefactoringApi.RenameProject.type,
      RenameProject.Params
    ],
    msg: Api.ProjectRenamed
  ): Unit = {
    val Api.ProjectRenamed(oldNormalizedName, newNormalizedName, newName) = msg
    replyTo ! ResponseResult(RenameProject, initialMsg.id, Unused)
    context.system.eventStream.publish(
      RefactoringProtocol.ProjectRenamedNotification(
        oldNormalizedName,
        newNormalizedName,
        newName
      )
    )
  }

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      RefactoringApi.RenameProject.type,
      RenameProject.Params
    ],
    error: Api.Error
  )(implicit ec: ExecutionContext): Unit = {
    val Api.ProjectRenameFailed(oldName, newName) = error
    replyTo ! ResponseError(
      Some(initialMsg.id),
      ProjectRenameFailed(oldName, newName)
    )
  }
}

object RenameProjectHandler {

  /** Creates configuration object used to create a [[RenameProjectHandler]].
    *
    * @param timeout request timeout
    * @param runtimeConnector reference to the runtime connector
    */
  def props(timeout: FiniteDuration, runtimeConnector: ActorRef): Props =
    Props(new RenameProjectHandler(timeout, runtimeConnector))

}

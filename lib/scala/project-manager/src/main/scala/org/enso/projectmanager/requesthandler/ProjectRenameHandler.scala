package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{Exec, Sync}
import org.enso.projectmanager.infrastructure.file.Files
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectRename
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.mapFailure
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/rename` commands.
  *
  * @param service a project service
  * @param requestTimeout a request timeout
  */
class ProjectRenameHandler[F[+_, +_]: Exec: CovariantFlatMap: Sync](
  service: ProjectServiceApi[F],
  requestTimeout: FiniteDuration
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ProjectRename, id, params: ProjectRename.Params) =>
      val action = for {
        projectsDirectory <-
          Sync[F].effect(params.projectsDirectory.map(Files.getAbsoluteFile))
        _ <- service.renameProject(
          params.projectId,
          params.name,
          projectsDirectory
        )
      } yield ()
      Exec[F].exec(action).pipeTo(self)
      val cancellable =
        context.system.scheduler
          .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      logger.error("Failure during ProjectRename operation.", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request {} with {} timed out.", ProjectRename, id)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: ProjectServiceFailure) =>
      logger.error(s"Request {} failed due to {}.", id, failure)
      replyTo ! ResponseError(Some(id), mapFailure(failure))
      cancellable.cancel()
      context.stop(self)

    case Right(()) =>
      replyTo ! ResponseResult(ProjectRename, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }

}

object ProjectRenameHandler {

  /** Creates a configuration object used to create a [[ProjectRenameHandler]].
    *
    * @param service a project service
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap: Sync](
    service: ProjectServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props =
    Props(new ProjectRenameHandler(service, requestTimeout))

}

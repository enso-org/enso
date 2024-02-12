package org.enso.projectmanager.requesthandler

import akka.actor._
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{Exec, Sync}
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectDelete
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.mapFailure
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}
import org.enso.projectmanager.util.UnhandledLogging

import java.io.File

import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/delete` commands.
  *
  * @param service a project service
  * @param requestTimeout a request timeout
  */
class ProjectDeleteHandler[F[+_, +_]: Exec: CovariantFlatMap: Sync](
  service: ProjectServiceApi[F],
  requestTimeout: FiniteDuration
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ProjectDelete, id, params: ProjectDelete.Params) =>
      val action = for {
        projectsDirectory <-
          Sync[F].effect(params.projectsDirectory.map(new File(_)))
        _ <- service.deleteUserProject(params.projectId, projectsDirectory)
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
      logger.error("Failure during ProjectDelete operation.", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request {} with {} timed out.", ProjectDelete, id)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: ProjectServiceFailure) =>
      logger.error("Request {} failed due to {}.", id, failure)
      replyTo ! ResponseError(Some(id), mapFailure(failure))
      cancellable.cancel()
      context.stop(self)

    case Right(()) =>
      replyTo ! ResponseResult(ProjectDelete, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }

}

object ProjectDeleteHandler {

  /** Creates a configuration object used to create a [[ProjectDeleteHandler]].
    *
    * @param service a project service
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap: Sync](
    service: ProjectServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props =
    Props(new ProjectDeleteHandler[F](service, requestTimeout))

}

package org.enso.projectmanager.requesthandler

import java.util.UUID

import akka.actor._
import akka.pattern.pipe
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectCreate
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.mapFailure
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}
import org.enso.projectmanager.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/create` commands.
  *
  * @param service a project service
  * @param requestTimeout a request timeout
  */
class ProjectCreateHandler[F[+_, +_]: Exec](
  service: ProjectServiceApi[F],
  requestTimeout: FiniteDuration
) extends Actor
    with ActorLogging
    with UnhandledLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ProjectCreate, id, params: ProjectCreate.Params) =>
      Exec[F].exec(service.createUserProject(params.name)).pipeTo(self)
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
      log.error(ex, s"Failure during $ProjectCreate operation:")
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $ProjectCreate with $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: ProjectServiceFailure) =>
      log.error(s"Request $id failed due to $failure")
      replyTo ! ResponseError(Some(id), mapFailure(failure))
      cancellable.cancel()
      context.stop(self)

    case Right(projectId: UUID) =>
      replyTo ! ResponseResult(
        ProjectCreate,
        id,
        ProjectCreate.Result(projectId)
      )
      cancellable.cancel()
      context.stop(self)
  }

}

object ProjectCreateHandler {

  /** Creates a configuration object used to create a [[ProjectCreateHandler]].
    *
    * @param service a project service
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec](
    service: ProjectServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props =
    Props(new ProjectCreateHandler(service, requestTimeout))

}

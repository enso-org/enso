package org.enso.projectmanager.requesthandler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Status}
import akka.pattern.pipe
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult}
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.data.ProjectMetadata
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectList
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.mapFailure
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}
import org.enso.projectmanager.util.UnhandledLogging

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/list` commands.
  *
  * @param clientId the requester id
  * @param service a project service
  * @param requestTimeout a request timeout
  */
class ProjectListHandler[F[+_, +_]: Exec](
  @unused clientId: UUID,
  service: ProjectServiceApi[F],
  requestTimeout: FiniteDuration
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ProjectList, id, params: ProjectList.Params) =>
      Exec[F]
        .exec { service.listProjects(params.numberOfProjects) }
        .pipeTo(self)
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
      log.error(ex, s"Failure during $ProjectList operation:")
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $ProjectList with $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: ProjectServiceFailure) =>
      log.error(s"Request $id failed due to $failure")
      replyTo ! ResponseError(Some(id), mapFailure(failure))
      cancellable.cancel()
      context.stop(self)

    case Right(list: List[_]) =>
      val metadata = list.collect { case meta: ProjectMetadata =>
        meta
      }

      replyTo ! ResponseResult(
        ProjectList,
        id,
        ProjectList.Result(metadata)
      )
      cancellable.cancel()
      context.stop(self)
  }

}

object ProjectListHandler {

  /** Creates a configuration object used to create a [[ProjectListHandler]].
    *
    * @param clientId the requester id
    * @param service a project service
    * @param requestTimeout a request timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec](
    clientId: UUID,
    service: ProjectServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props =
    Props(new ProjectListHandler(clientId, service, requestTimeout))

}

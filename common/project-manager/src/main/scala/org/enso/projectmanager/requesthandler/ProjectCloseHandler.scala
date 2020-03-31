package org.enso.projectmanager.requesthandler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Status}
import akka.pattern.pipe
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectClose
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.mapFailure
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `project/close` commands.
  *
  * @param clientId the requester id
  * @param service a project service
  * @param requestTimeout a request timeout
  */
class ProjectCloseHandler[F[+_, +_]: Exec](
  clientId: UUID,
  service: ProjectServiceApi[F],
  requestTimeout: FiniteDuration
) extends Actor
    with ActorLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(ProjectClose, id, params: ProjectClose.Params) =>
      Exec[F]
        .exec(service.closeProject(clientId, params.projectId))
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
      log.error(s"Failure during $ProjectClose operation:", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $ProjectClose with $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case Left(failure: ProjectServiceFailure) =>
      log.error(s"Request $id failed due to $failure")
      replyTo ! ResponseError(Some(id), mapFailure(failure))
      cancellable.cancel()
      context.stop(self)

    case Right(()) =>
      replyTo ! ResponseResult(ProjectClose, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }

}

object ProjectCloseHandler {

  /**
    * Creates a configuration object used to create a [[ProjectCloseHandler]].
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
    Props(new ProjectCloseHandler(clientId, service, requestTimeout))

}

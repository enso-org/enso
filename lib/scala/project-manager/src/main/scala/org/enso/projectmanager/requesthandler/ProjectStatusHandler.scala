package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.data.RunningStatus
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectStatus
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}

import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/status` commands.
  *
  * @param clientId the requester id
  * @param projectService a project service
  * @param requestTimeout a request timeout
  * @param timeoutRetries a number of timeouts to wait until a failure is reported
  */
class ProjectStatusHandler[F[+_, +_]: Exec: CovariantFlatMap](
  clientId: UUID,
  projectService: ProjectServiceApi[F],
  requestTimeout: FiniteDuration,
  timeoutRetries: Int
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      ProjectStatus.type,
      ProjectStatus.Params,
      ProjectStatus.Result
    ](
      ProjectStatus,
      Some(requestTimeout),
      timeoutRetries
    ) {

  override def handleRequest = { params =>
    for {
      server <- projectService.getProjectStatus(
        clientId  = clientId,
        projectId = params.projectId
      )
    } yield ProjectStatus.Result(status =
      RunningStatus(server.open, server.shuttingDown)
    )
  }

}

object ProjectStatusHandler {

  /** Creates a configuration object used to create a [[ProjectStatusHandler]].
    *
    * @param clientId the requester id
    * @param projectService a project service
    * @param requestTimeout a request timeout
    * @param timeoutRetries a number of timeouts to wait until a failure is reported
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    clientId: UUID,
    projectService: ProjectServiceApi[F],
    requestTimeout: FiniteDuration,
    timeoutRetries: Int
  ): Props =
    Props(
      new ProjectStatusHandler(
        clientId,
        projectService,
        requestTimeout,
        timeoutRetries
      )
    )

}

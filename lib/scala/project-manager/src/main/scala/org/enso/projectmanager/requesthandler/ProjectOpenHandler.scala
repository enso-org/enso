package org.enso.projectmanager.requesthandler

import java.util.UUID

import akka.actor.Props
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectOpen
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/open` commands.
  *
  * @param clientId the requester id
  * @param projectService a project service
  * @param requestTimeout a request timeout
  * @param timeoutRetries a number of timeouts to wait until a failure is reported
  */
class ProjectOpenHandler[F[+_, +_]: Exec: CovariantFlatMap](
  clientId: UUID,
  projectService: ProjectServiceApi[F],
  requestTimeout: FiniteDuration,
  timeoutRetries: Int
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      ProjectOpen.type,
      ProjectOpen.Params,
      ProjectOpen.Result
    ](
      ProjectOpen,
      // TODO [RW] maybe we can get rid of this timeout since boot timeout is
      //  handled by the LanguageServerProcess; still the ? message of
      //  LanguageServerGateway will result in timeouts (#1315)
      Some(requestTimeout),
      timeoutRetries
    ) {

  override def handleRequest = { params =>
    val missingComponentAction =
      params.missingComponentAction.getOrElse(MissingComponentAction.Fail)

    for {
      server <- projectService.openProject(
        progressTracker        = self,
        clientId               = clientId,
        projectId              = params.projectId,
        missingComponentAction = missingComponentAction
      )
    } yield ProjectOpen.Result(
      engineVersion               = server.engineVersion,
      languageServerJsonAddress   = server.sockets.jsonSocket,
      languageServerBinaryAddress = server.sockets.binarySocket,
      projectName                 = server.projectName,
      projectNormalizedName       = server.projectNormalizedName,
      projectNamespace            = server.projectNamespace
    )
  }

}

object ProjectOpenHandler {

  /** Creates a configuration object used to create a [[ProjectOpenHandler]].
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
      new ProjectOpenHandler(
        clientId,
        projectService,
        requestTimeout,
        timeoutRetries
      )
    )

}

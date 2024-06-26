package org.enso.projectmanager.requesthandler

import akka.actor._
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{Exec, Sync}
import org.enso.projectmanager.infrastructure.file.Files
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectDuplicate
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/duplicate` commands.
  *
  * @param projectService a project service
  * @param requestTimeout a request timeout
  * @param timeoutRetries a number of timeouts to wait until a failure is reported
  */
class ProjectDuplicateHandler[
  F[+_, +_]: Exec: CovariantFlatMap: Sync
](
  projectService: ProjectServiceApi[F],
  requestTimeout: FiniteDuration,
  timeoutRetries: Int
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      ProjectDuplicate.type,
      ProjectDuplicate.Params,
      ProjectDuplicate.Result
    ](
      ProjectDuplicate,
      Some(requestTimeout),
      timeoutRetries
    ) {

  override def handleRequest: ProjectDuplicate.Params => F[
    ProjectServiceFailure,
    ProjectDuplicate.Result
  ] = { params =>
    for {
      projectsDirectory <- Sync[F].effect(
        params.projectsDirectory.map(Files.getAbsoluteFile)
      )
      project <- projectService.duplicateUserProject(
        projectId         = params.projectId,
        projectsDirectory = projectsDirectory
      )
      _ = logger.trace(
        "Duplicated requested project {} with new name {}",
        params.projectId,
        project.name
      )
    } yield ProjectDuplicate.Result(project.id, project.name, project.module)
  }
}

object ProjectDuplicateHandler {

  /** Creates a configuration object used to create a [[ProjectDuplicateHandler]].
    *
    * @param projectService a project service
    * @param requestTimeout a request timeout
    * @param timeoutRetries a number of timeouts to wait until a failure is reported
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap: Sync](
    projectService: ProjectServiceApi[F],
    requestTimeout: FiniteDuration,
    timeoutRetries: Int
  ): Props =
    Props(
      new ProjectDuplicateHandler(
        projectService,
        requestTimeout,
        timeoutRetries
      )
    )

}

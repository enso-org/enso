package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.data.ProjectMetadata
import org.enso.projectmanager.protocol.ProjectManagementApi.ProjectList
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.{
  ProjectServiceApi,
  ProjectServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/** A request handler for `project/list` commands.
  *
  * @param clientId the requester id
  * @param service a project service
  * @param requestTimeout a request timeout
  * @param timeoutRetries a number of timeouts to wait until a failure is reported
  */
class ProjectListHandler[F[+_, +_]: Exec: CovariantFlatMap](
  projectService: ProjectServiceApi[F],
  requestTimeout: FiniteDuration,
  timeoutRetries: Int
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      ProjectList.type,
      ProjectList.Params,
      ProjectList.Result
    ](
      ProjectList,
      Some(requestTimeout),
      timeoutRetries
    ) {

  override def handleRequest = { params =>
    for {
      projects <- projectService.listProjects(params.numberOfProjects)
    } yield ProjectList.Result(projects.collect { case meta: ProjectMetadata =>
      meta
    })
  }

}

object ProjectListHandler {

  /** Creates a configuration object used to create a [[ProjectListHandler]].
    *
    * @param clientId the requester id
    * @param service a project service
    * @param requestTimeout a request timeout
    * @param timeoutRetries a number of timeouts to wait until a failure is reported
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: ProjectServiceApi[F],
    requestTimeout: FiniteDuration,
    timeoutRetries: Int
  ): Props =
    Props(
      new ProjectListHandler(service, requestTimeout, timeoutRetries)
    )

}

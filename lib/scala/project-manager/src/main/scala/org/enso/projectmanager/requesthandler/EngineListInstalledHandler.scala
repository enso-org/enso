package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.EngineListInstalled
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi

import scala.concurrent.duration.FiniteDuration

/** A request handler for `engine/list-installed` commands.
  *
  * @param service a project service
  * @param requestTimeout timeout
  */
class EngineListInstalledHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: RuntimeVersionManagementServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[F, ProjectServiceFailure](
      EngineListInstalled,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest
    : PartialFunction[Any, F[ProjectServiceFailure, Any]] = {
    case Request(EngineListInstalled, id, Unused) =>
      for {
        result <- service.listInstalledEngines()
        sorted = result.sortBy(_.version).reverse
      } yield ResponseResult(
        EngineListInstalled,
        id,
        EngineListInstalled.Result(sorted)
      )
  }
}

object EngineListInstalledHandler {

  /** Creates a configuration object used to create a
    * [[EngineListInstalledHandler]].
    *
    * @param service a runtime version management service
    * @param requestTimeout timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: RuntimeVersionManagementServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new EngineListInstalledHandler[F](service, requestTimeout))
}

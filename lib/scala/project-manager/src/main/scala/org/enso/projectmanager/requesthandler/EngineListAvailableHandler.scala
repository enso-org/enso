package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.EngineListAvailable
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi

import scala.concurrent.duration.FiniteDuration

class EngineListAvailableHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: RuntimeVersionManagementServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[F, ProjectServiceFailure](
      EngineListAvailable,
      Some(requestTimeout)
    ) {
  override def handleRequest
    : PartialFunction[Any, F[ProjectServiceFailure, Any]] = {
    case Request(EngineListAvailable, id, Unused) =>
      for {
        result <- service.listAvailableEngines()
      } yield ResponseResult(
        EngineListAvailable,
        id,
        EngineListAvailable.Result(result)
      )
  }
}

object EngineListAvailableHandler {
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: RuntimeVersionManagementServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new EngineListAvailableHandler[F](service, requestTimeout))
}

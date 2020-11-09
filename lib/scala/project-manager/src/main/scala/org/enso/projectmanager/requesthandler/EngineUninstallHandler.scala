package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.EngineUninstall
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi

class EngineUninstallHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: RuntimeVersionManagementServiceApi[F]
) extends RequestHandler[F, ProjectServiceFailure](EngineUninstall, None) {
  override def handleRequest
    : PartialFunction[Any, F[ProjectServiceFailure, Any]] = {
    case Request(EngineUninstall, id, params: EngineUninstall.Params) =>
      val progressTracker = sender()
      for {
        _ <- service.uninstallEngine(progressTracker, params.version)
      } yield ResponseResult(EngineUninstall, id, Unused)
  }
}

object EngineUninstallHandler {
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: RuntimeVersionManagementServiceApi[F]
  ): Props = Props(new EngineUninstallHandler[F](service))
}

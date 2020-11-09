package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ConfigDelete
import org.enso.projectmanager.service.config.{
  GlobalConfigServiceApi,
  GlobalConfigServiceFailure
}

import scala.concurrent.duration.FiniteDuration

class ConfigDeleteHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: GlobalConfigServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[F, GlobalConfigServiceFailure](
      ConfigDelete,
      Some(requestTimeout)
    ) {
  override def handleRequest
    : PartialFunction[Any, F[GlobalConfigServiceFailure, Any]] = {
    case Request(ConfigDelete, id, params: ConfigDelete.Params) =>
      for {
        _ <- service.deleteKey(params.key)
      } yield ResponseResult(ConfigDelete, id, Unused)
  }
}

object ConfigDeleteHandler {
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: GlobalConfigServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new ConfigDeleteHandler[F](service, requestTimeout))
}

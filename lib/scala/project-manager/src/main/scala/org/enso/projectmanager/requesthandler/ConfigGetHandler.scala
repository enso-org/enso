package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.{Request, ResponseResult}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ConfigGet
import org.enso.projectmanager.service.config.{
  GlobalConfigServiceApi,
  GlobalConfigServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/** A request handler for `global-config/get` commands.
  *
  * @param service a project service
  * @param requestTimeout timeout
  */
class ConfigGetHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: GlobalConfigServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[F, GlobalConfigServiceFailure](
      ConfigGet,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest
    : PartialFunction[Any, F[GlobalConfigServiceFailure, Any]] = {
    case Request(ConfigGet, id, params: ConfigGet.Params) =>
      for {
        value <- service.getKey(params.key)
      } yield ResponseResult(ConfigGet, id, ConfigGet.Result(value))
  }
}

object ConfigGetHandler {

  /** Creates a configuration object used to create a
    * [[ConfigGetHandler]].
    *
    * @param service a runtime version management service
    * @param requestTimeout timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: GlobalConfigServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new ConfigGetHandler[F](service, requestTimeout))
}

package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ConfigSet
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
class ConfigSetHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: GlobalConfigServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[F, GlobalConfigServiceFailure](
      ConfigSet,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest
    : PartialFunction[Any, F[GlobalConfigServiceFailure, Any]] = {
    case Request(ConfigSet, id, params: ConfigSet.Params) =>
      for {
        _ <- service.setKey(params.key, params.value)
      } yield ResponseResult(ConfigSet, id, Unused)
  }
}

object ConfigSetHandler {

  /** Creates a configuration object used to create a
    * [[ConfigSetHandler]].
    *
    * @param service a runtime version management service
    * @param requestTimeout timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: GlobalConfigServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new ConfigSetHandler[F](service, requestTimeout))
}

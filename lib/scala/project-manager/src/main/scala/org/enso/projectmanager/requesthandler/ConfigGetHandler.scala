package org.enso.projectmanager.requesthandler

import akka.actor.Props
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
) extends RequestHandler[
      F,
      GlobalConfigServiceFailure,
      ConfigGet.type,
      ConfigGet.Params,
      ConfigGet.Result
    ](
      ConfigGet,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest = { params =>
    for {
      value <- service.getKey(params.key)
    } yield ConfigGet.Result(value)
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

package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.Unused
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
) extends RequestHandler[
      F,
      GlobalConfigServiceFailure,
      ConfigSet.type,
      ConfigSet.Params,
      Unused.type
    ](
      ConfigSet,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest = { params =>
    for {
      _ <- service.setKey(params.key, params.value)
    } yield Unused
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

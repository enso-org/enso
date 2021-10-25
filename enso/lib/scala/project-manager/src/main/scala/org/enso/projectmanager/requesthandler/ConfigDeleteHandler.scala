package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.Unused
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.ConfigDelete
import org.enso.projectmanager.service.config.{
  GlobalConfigServiceApi,
  GlobalConfigServiceFailure
}

import scala.concurrent.duration.FiniteDuration

/** A request handler for `global-config/delete` commands.
  *
  * @param service a project service
  * @param requestTimeout timeout
  */
class ConfigDeleteHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: GlobalConfigServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[
      F,
      GlobalConfigServiceFailure,
      ConfigDelete.type,
      ConfigDelete.Params,
      Unused.type
    ](
      ConfigDelete,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest = { params =>
    for {
      _ <- service.deleteKey(params.key)
    } yield Unused
  }
}

object ConfigDeleteHandler {

  /** Creates a configuration object used to create a
    * [[ConfigDeleteHandler]].
    *
    * @param service a runtime version management service
    * @param requestTimeout timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: GlobalConfigServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new ConfigDeleteHandler[F](service, requestTimeout))
}

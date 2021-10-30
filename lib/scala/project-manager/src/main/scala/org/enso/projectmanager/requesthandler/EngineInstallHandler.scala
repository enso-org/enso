package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.Unused
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.EngineInstall
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi

/** A request handler for `engine/install` commands.
  *
  * @param service a project service
  */
class EngineInstallHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: RuntimeVersionManagementServiceApi[F]
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      EngineInstall.type,
      EngineInstall.Params,
      Unused.type
    ](EngineInstall, None) {

  /** @inheritdoc */
  override def handleRequest = { params =>
    for {
      _ <- service.installEngine(
        progressTracker    = self,
        version            = params.version,
        forceInstallBroken = params.forceInstallBroken.getOrElse(false)
      )
    } yield Unused
  }
}

object EngineInstallHandler {

  /** Creates a configuration object used to create a [[EngineInstallHandler]].
    *
    * @param service a runtime version management service
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: RuntimeVersionManagementServiceApi[F]
  ): Props = Props(new EngineInstallHandler[F](service))
}

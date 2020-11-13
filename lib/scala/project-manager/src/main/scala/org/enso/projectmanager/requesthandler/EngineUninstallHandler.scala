package org.enso.projectmanager.requesthandler

import akka.actor.Props
import org.enso.jsonrpc.Unused
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.EngineUninstall
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi

/** A request handler for `engine/uninstall` commands.
  *
  * @param service a project service
  */
class EngineUninstallHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: RuntimeVersionManagementServiceApi[F]
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      EngineUninstall.type,
      EngineUninstall.Params,
      Unused.type
    ](
      EngineUninstall,
      None
    ) {

  /** @inheritdoc */
  override def handleRequest = { params =>
    val progressTracker = sender()
    for {
      _ <- service.uninstallEngine(progressTracker, params.version)
    } yield Unused
  }
}

object EngineUninstallHandler {

  /** Creates a configuration object used to create a
    * [[EngineUninstallHandler]].
    *
    * @param service a runtime version management service
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: RuntimeVersionManagementServiceApi[F]
  ): Props = Props(new EngineUninstallHandler[F](service))
}

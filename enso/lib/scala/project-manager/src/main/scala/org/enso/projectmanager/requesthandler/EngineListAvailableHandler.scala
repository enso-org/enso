package org.enso.projectmanager.requesthandler

import akka.actor.Props
import nl.gn0s1s.bump.SemVer
import org.enso.jsonrpc.Unused
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.Exec
import org.enso.projectmanager.protocol.ProjectManagementApi.EngineListAvailable
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper.failureMapper
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementServiceApi

import scala.concurrent.duration.FiniteDuration

/** A request handler for `engine/list-available` commands.
  *
  * @param service a project service
  * @param requestTimeout timeout
  */
class EngineListAvailableHandler[F[+_, +_]: Exec: CovariantFlatMap](
  service: RuntimeVersionManagementServiceApi[F],
  requestTimeout: FiniteDuration
) extends RequestHandler[
      F,
      ProjectServiceFailure,
      EngineListAvailable.type,
      Unused.type,
      EngineListAvailable.Result
    ](
      EngineListAvailable,
      Some(requestTimeout)
    ) {

  /** @inheritdoc */
  override def handleRequest = { _ =>
    for {
      result <- service.listAvailableEngines()
      sorted = result.sortBy(_.version)(Ordering[SemVer].reverse)
    } yield EngineListAvailable.Result(sorted)
  }
}

object EngineListAvailableHandler {

  /** Creates a configuration object used to create a
    * [[EngineListAvailableHandler]].
    *
    * @param service a runtime version management service
    * @param requestTimeout timeout
    * @return a configuration object
    */
  def props[F[+_, +_]: Exec: CovariantFlatMap](
    service: RuntimeVersionManagementServiceApi[F],
    requestTimeout: FiniteDuration
  ): Props = Props(new EngineListAvailableHandler[F](service, requestTimeout))
}

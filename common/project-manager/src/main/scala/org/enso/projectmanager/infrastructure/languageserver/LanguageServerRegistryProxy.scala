package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{Async, ErrorChannel}
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.model.Project

/**
  * It is a proxy to actor based language subsystem. It a bridge between
  * actor interface and pure functional effects.
  *
  * @param registry a lang. server registry
  * @param timeoutConfig a timeout config
  * @tparam F a effectful context
  */
class LanguageServerRegistryProxy[F[+_, +_]: Async: ErrorChannel: CovariantFlatMap](
  registry: ActorRef,
  timeoutConfig: TimeoutConfig
) extends LanguageServerService[F] {

  implicit val timeout: Timeout = Timeout(timeoutConfig.bootTimeout)

  /** @inheritdoc **/
  override def start(
    clientId: UUID,
    project: Project
  ): F[ServerStartupFailure, Socket] =
    Async[F]
      .fromFuture { () =>
        (registry ? StartServer(clientId, project)).mapTo[ServerStartupResult]
      }
      .mapError(_ => ServerBootTimedOut)
      .flatMap {
        case ServerStarted(socket)   => CovariantFlatMap[F].pure(socket)
        case f: ServerStartupFailure => ErrorChannel[F].fail(f)
      }

  /** @inheritdoc **/
  override def stop(
    clientId: UUID,
    projectId: UUID
  ): F[ServerStoppageFailure, Unit] =
    Async[F]
      .fromFuture { () =>
        (registry ? StopServer(clientId, projectId)).mapTo[ServerStoppageResult]
      }
      .mapError(FailureDuringStoppage(_))
      .flatMap {
        case ServerStopped            => CovariantFlatMap[F].pure()
        case f: ServerStoppageFailure => ErrorChannel[F].fail(f)
      }

  /** @inheritdoc **/
  override def isRunning(projectId: UUID): F[CheckTimeout.type, Boolean] =
    Async[F]
      .fromFuture { () =>
        (registry ? CheckIfServerIsRunning(projectId)).mapTo[Boolean]
      }
      .mapError(_ => CheckTimeout)

}

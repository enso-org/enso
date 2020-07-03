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
import org.enso.projectmanager.data.LanguageServerSockets
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
class LanguageServerRegistryProxy[
  F[+_, +_]: Async: ErrorChannel: CovariantFlatMap
](
  registry: ActorRef,
  timeoutConfig: TimeoutConfig
) extends LanguageServerService[F] {

  /** @inheritdoc * */
  override def start(
    clientId: UUID,
    project: Project
  ): F[ServerStartupFailure, LanguageServerSockets] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.bootTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? StartServer(clientId, project)).mapTo[ServerStartupResult]
      }
      .mapError(_ => ServerBootTimedOut)
      .flatMap {
        case ServerStarted(sockets)  => CovariantFlatMap[F].pure(sockets)
        case f: ServerStartupFailure => ErrorChannel[F].fail(f)
      }
  }

  /** @inheritdoc * */
  override def stop(
    clientId: UUID,
    projectId: UUID
  ): F[ServerShutdownFailure, Unit] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.shutdownTimeout)
    Async[F]
      .fromFuture { () =>
        (registry ? StopServer(clientId, projectId)).mapTo[ServerShutdownResult]
      }
      .mapError(FailureDuringShutdown)
      .flatMap {
        case ServerStopped            => CovariantFlatMap[F].pure(())
        case f: ServerShutdownFailure => ErrorChannel[F].fail(f)
      }
  }

  /** @inheritdoc * */
  override def isRunning(projectId: UUID): F[CheckTimeout.type, Boolean] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.requestTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? CheckIfServerIsRunning(projectId)).mapTo[Boolean]
      }
      .mapError(_ => CheckTimeout)
  }

  /** @inheritdoc * */
  override def renameProject(
    projectId: UUID,
    oldName: String,
    newName: String
  ): F[ProjectRenameFailure, Unit] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.requestTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? RenameProject(projectId, oldName, newName))
          .mapTo[ProjectRenameResult]
      }
      .mapError(_ => RenameTimeout)
      .flatMap {
        case ProjectRenamed          => CovariantFlatMap[F].pure(())
        case f: ProjectRenameFailure => ErrorChannel[F].fail(f)
      }
  }

  /** @inheritdoc * */
  override def killAllServers(): F[Nothing, Boolean] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.shutdownTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? KillThemAll).mapTo[AllServersKilled.type]
      }
      .map(_ => true)
      .fallbackTo(_ => CovariantFlatMap[F].pure(false))
  }
}

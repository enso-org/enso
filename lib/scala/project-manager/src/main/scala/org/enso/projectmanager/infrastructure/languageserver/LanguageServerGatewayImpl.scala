package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.boot.configuration.TimeoutConfig
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{Async, ErrorChannel, Sync}
import org.enso.projectmanager.data.LanguageServerSockets
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivationWatcher.{
  AllShutdownHooksFired,
  Watch
}
import org.enso.projectmanager.infrastructure.languageserver.ShutdownHookActivator.RegisterShutdownHook
import org.enso.projectmanager.infrastructure.shutdown.ShutdownHook
import org.enso.projectmanager.model.Project

/** It is a proxy to actor based language subsystem. It a bridge between
  * actor interface and pure functional effects.
  *
  * @param registry a lang. server registry
  * @param timeoutConfig a timeout config
  * @tparam F a effectful context
  */
class LanguageServerGatewayImpl[
  F[+_, +_]: Async: Sync: ErrorChannel: CovariantFlatMap
](
  registry: ActorRef,
  shutdownHookActivator: ActorRef,
  actorSystem: ActorSystem,
  timeoutConfig: TimeoutConfig
) extends LanguageServerGateway[F] {

  /** @inheritdoc */
  override def start(
    progressTracker: ActorRef,
    clientId: UUID,
    project: Project,
    version: SemVer
  ): F[ServerStartupFailure, LanguageServerSockets] = {
    implicit val timeout: Timeout = Timeout(2 * timeoutConfig.bootTimeout)

    // TODO [RW] this can timeout if the boot is stuck waiting on a lock, how do
    //  we want to handle that? #1315
    Async[F]
      .fromFuture { () =>
        (registry ? StartServer(
          clientId,
          project,
          version,
          progressTracker,
          engineUpdate = false
        )).mapTo[ServerStartupResult]
      }
      .mapError(_ => ServerBootTimedOut)
      .flatMap {
        case ServerStarted(sockets)  => CovariantFlatMap[F].pure(sockets)
        case f: ServerStartupFailure => ErrorChannel[F].fail(f)
      }
  }

  /** @inheritdoc */
  override def stop(
    clientId: UUID,
    projectId: UUID
  ): F[ServerShutdownFailure, Unit] = {
    implicit val timeout: Timeout = Timeout(
      timeoutConfig.shutdownTimeout + timeoutConfig.delayedShutdownTimeout
    )
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

  /** @inheritdoc */
  override def isRunning(projectId: UUID): F[CheckTimeout.type, Boolean] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.requestTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? CheckIfServerIsRunning(projectId)).mapTo[Boolean]
      }
      .mapError(_ => CheckTimeout)
  }

  /** @inheritdoc */
  override def renameProject(
    projectId: UUID,
    namespace: String,
    oldName: String,
    newName: String
  ): F[ProjectRenameFailure, Unit] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.requestTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? RenameProject(projectId, namespace, oldName, newName))
          .mapTo[ProjectRenameResult]
      }
      .mapError(_ => RenameTimeout)
      .flatMap {
        case ProjectRenamed          => CovariantFlatMap[F].pure(())
        case f: ProjectRenameFailure => ErrorChannel[F].fail(f)
      }
  }

  /** @inheritdoc */
  override def killAllServers(): F[Throwable, Boolean] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.shutdownTimeout)

    Async[F]
      .fromFuture { () =>
        (registry ? KillThemAll).mapTo[AllServersKilled.type]
      }
      .map(_ => true)
  }

  /** @inheritdoc */
  override def registerShutdownHook(
    projectId: UUID,
    hook: ShutdownHook[F]
  ): F[Nothing, Unit] = {
    Sync[F]
      .effect {
        shutdownHookActivator ! RegisterShutdownHook(projectId, hook)
      }
  }

  /** @inheritdoc */
  override def waitTillAllHooksFired(): F[Throwable, Unit] = {
    implicit val timeout: Timeout = Timeout(timeoutConfig.shutdownTimeout)
    val watcher = actorSystem.actorOf(
      ShutdownHookActivationWatcher.props(shutdownHookActivator)
    )

    Async[F]
      .fromFuture { () =>
        (watcher ? Watch).mapTo[AllShutdownHooksFired.type]
      }
      .map(_ => ())
  }

}

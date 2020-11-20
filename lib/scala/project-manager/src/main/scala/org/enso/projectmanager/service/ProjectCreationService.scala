package org.enso.projectmanager.service

import java.nio.file.Path

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.service.ProjectServiceFailure.ProjectCreateFailed
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerMixin
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.runner.Runner

import scala.concurrent.Future

class ProjectCreationService[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap
](
  override val distributionConfiguration: DistributionConfiguration
) extends ProjectCreationServiceApi[F]
    with RuntimeVersionManagerMixin {

  override def createProject(
    progressTracker: ActorRef,
    path: Path,
    name: String,
    version: SemVer,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, Unit] = Sync[F]
    .blockingOp {
      val versionManager =
        makeRuntimeVersionManager(progressTracker, missingComponentAction)
      val runner =
        new Runner(
          versionManager,
          distributionConfiguration.environment,
          Future.successful(None)
        )

      val settings =
        runner.newProject(path, name, version, None, None, Seq()).get
      val jvmSettings = distributionConfiguration.defaultJVMSettings
      runner.withCommand(settings, jvmSettings) { command =>
        command.run().get
      }
    }
    .mapRuntimeManagerErrors { other: Throwable =>
      ProjectCreateFailed(other.getMessage)
    }
    .flatMap { exitCode =>
      if (exitCode == 0)
        CovariantFlatMap[F].pure(())
      else
        ErrorChannel[F].fail(
          ProjectCreateFailed(
            s"The runner used to create the project returned exit code " +
            s"$exitCode."
          )
        )
    }
}

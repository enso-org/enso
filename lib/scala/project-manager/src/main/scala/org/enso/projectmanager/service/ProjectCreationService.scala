package org.enso.projectmanager.service

import akka.actor.ActorRef
import com.typesafe.scalalogging.Logger
import org.enso.semver.SemVer
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.data.MissingComponentActions
import org.enso.projectmanager.service.ProjectServiceFailure.ProjectCreateFailed
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerErrorRecoverySyntax._
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerFactory
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.config.GlobalRunnerConfigurationManager
import org.enso.runtimeversionmanager.runner.Runner
import java.nio.file.Path

/** A service for creating new project structures using the runner of the
  * specific engine version selected for the project.
  */
class ProjectCreationService[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap
](
  distributionConfiguration: DistributionConfiguration,
  loggingServiceDescriptor: LoggingServiceDescriptor
) extends ProjectCreationServiceApi[F] {

  private lazy val logger = Logger[ProjectCreationService[F]]

  /** @inheritdoc */
  override def createProject(
    progressTracker: ActorRef,
    path: Path,
    name: String,
    engineVersion: SemVer,
    projectTemplate: Option[String],
    missingComponentAction: MissingComponentActions.MissingComponentAction
  ): F[ProjectServiceFailure, Unit] = Sync[F]
    .blockingOp {
      val versionManager = RuntimeVersionManagerFactory(
        distributionConfiguration
      ).makeRuntimeVersionManager(progressTracker, missingComponentAction)
      val configurationManager = new GlobalRunnerConfigurationManager(
        versionManager,
        distributionConfiguration.distributionManager
      )
      val runner =
        new Runner(
          runtimeVersionManager      = versionManager,
          distributionManager        = distributionConfiguration.distributionManager,
          globalConfigurationManager = configurationManager,
          editionManager             = distributionConfiguration.editionManager,
          environment                = distributionConfiguration.environment,
          loggerConnection           = loggingServiceDescriptor.getEndpoint
        )

      val settings =
        runner
          .newProject(
            path,
            name,
            engineVersion,
            None,
            projectTemplate,
            None,
            None,
            Seq()
          )
          .get
      val jvmSettings = distributionConfiguration.defaultJVMSettings
      runner.withCommand(settings, jvmSettings) { command =>
        logger.error(
          "[ProjectCreationService.createProject] Runner arguments = {}, " +
          "jvmSettings.jvmOptions = {}, jvmSettings.extraOptions = {}, " +
          "jvmSettings.javaCommandOverride = {}",
          settings.runnerArguments,
          jvmSettings.jvmOptions,
          jvmSettings.extraOptions,
          jvmSettings.javaCommandOverride
        )
        command.runAndCaptureOutput().get
      }
    }
    .mapRuntimeManagerErrors { other: Throwable =>
      logger.error("[ProjectCreationService.createProject] Error: {}", other)
      ProjectCreateFailed(other.getMessage)
    }
    .flatMap { case (exitCode, output) =>
      if (exitCode == 0)
        CovariantFlatMap[F].pure(())
      else
        ErrorChannel[F].fail(
          ProjectCreateFailed(
            s"The runner used to create the project returned exit code $exitCode.${System
              .getProperty("line.separator")}Runner's output: $output"
          )
        )
    }
}

package org.enso.projectmanager.service.versionmanagement

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.data.EngineVersion
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.ProjectServiceFailure.{
  ComponentInstallationFailure,
  ComponentRepositoryAccessFailure,
  ComponentUninstallationFailure
}
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerErrorRecoverySyntax._
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.components.ComponentMissingError

/** A facade for runtime version management logic that processes the requests
  * using the [[RuntimeVersionManager]].
  *
  * @param distributionConfiguration a distribution configuration
  */
class RuntimeVersionManagementService[F[+_, +_]: Sync: ErrorChannel](
  distributionConfiguration: DistributionConfiguration
) extends RuntimeVersionManagementServiceApi[F] {

  val factory = RuntimeVersionManagerFactory(distributionConfiguration)

  /** @inheritdoc */
  override def installEngine(
    progressTracker: ActorRef,
    version: SemVer,
    forceInstallBroken: Boolean
  ): F[ProjectServiceFailure, Unit] = {
    Sync[F]
      .blockingOp {
        factory
          .makeRuntimeVersionManager(
            progressTracker,
            allowMissingComponents = true,
            allowBrokenComponents  = forceInstallBroken
          )
          .findOrInstallEngine(version)
        ()
      }
      .mapRuntimeManagerErrors(throwable =>
        ComponentInstallationFailure(throwable.getMessage)
      )
  }

  /** @inheritdoc */
  override def uninstallEngine(
    progressTracker: ActorRef,
    version: SemVer
  ): F[ProjectServiceFailure, Unit] = Sync[F]
    .blockingOp {
      try {
        factory
          .makeRuntimeVersionManager(
            progressTracker,
            allowMissingComponents = false,
            allowBrokenComponents  = false
          )
          .uninstallEngine(version)
      } catch {
        case _: ComponentMissingError =>
      }
    }
    .mapRuntimeManagerErrors(throwable =>
      ComponentUninstallationFailure(throwable.getMessage)
    )

  /** @inheritdoc */
  override def listInstalledEngines()
    : F[ProjectServiceFailure, Seq[EngineVersion]] = Sync[F]
    .blockingOp {
      factory.makeReadOnlyVersionManager().listInstalledEngines().map {
        installedEngine =>
          EngineVersion(installedEngine.version, installedEngine.isMarkedBroken)
      }
    }
    .mapRuntimeManagerErrors(throwable =>
      ComponentRepositoryAccessFailure(throwable.getMessage)
    )

  /** @inheritdoc */
  override def listAvailableEngines()
    : F[ProjectServiceFailure, Seq[EngineVersion]] = Sync[F]
    .blockingOp {
      val engineReleaseProvider =
        distributionConfiguration.engineReleaseProvider
      engineReleaseProvider.fetchAllVersions().get.map { availableEngine =>
        EngineVersion(availableEngine.version, availableEngine.markedAsBroken)
      }
    }
    .mapRuntimeManagerErrors(throwable =>
      ComponentRepositoryAccessFailure(throwable.getMessage)
    )
}

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
import org.enso.projectmanager.versionmanagement.DistributionManagementConfiguration

class RuntimeVersionManagementService[F[+_, +_]: Sync: ErrorChannel](
  override val managers: DistributionManagementConfiguration
) extends RuntimeVersionManagementServiceApi[F]
    with RuntimeVersionManagerMixin {

  override def installEngine(
    progressTracker: ActorRef,
    version: SemVer,
    forceInstallBroken: Boolean
  ): F[ProjectServiceFailure, Unit] = {
    Sync[F]
      .blockingIO {
        makeRuntimeVersionManager(
          progressTracker,
          allowMissingComponents = true,
          allowBrokenComponents  = forceInstallBroken
        ).findOrInstallEngine(version)
        ()
      }
      .recoverAccessErrors(throwable =>
        ComponentInstallationFailure(throwable.getMessage)
      )
  }

  override def uninstallEngine(
    progressTracker: ActorRef,
    version: SemVer
  ): F[ProjectServiceFailure, Unit] = Sync[F]
    .blockingIO {
      makeRuntimeVersionManager(
        progressTracker,
        allowMissingComponents = false,
        allowBrokenComponents  = false
      ).uninstallEngine(version)
    }
    .recoverAccessErrors(throwable =>
      ComponentUninstallationFailure(throwable.getMessage)
    )

  override def listInstalledEngines()
    : F[ProjectServiceFailure, Seq[EngineVersion]] = Sync[F]
    .blockingIO {
      makeReadOnlyVersionManager().listInstalledEngines().map {
        installedEngine =>
          EngineVersion(installedEngine.version, installedEngine.isMarkedBroken)
      }
    }
    .recoverAccessErrors(throwable =>
      ComponentRepositoryAccessFailure(throwable.getMessage)
    )

  override def listAvailableEngines()
    : F[ProjectServiceFailure, Seq[EngineVersion]] = Sync[F]
    .blockingIO {
      val engineReleaseProvider = managers.engineReleaseProvider
      engineReleaseProvider.fetchAllVersions().get.map { availableEngine =>
        EngineVersion(availableEngine.version, availableEngine.markedAsBroken)
      }
    }
    .recoverAccessErrors(throwable =>
      ComponentRepositoryAccessFailure(throwable.getMessage)
    )
}

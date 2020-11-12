package org.enso.projectmanager.service.versionmanagement

import akka.actor.ActorRef
import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.ProjectServiceFailure.{
  BrokenComponentFailure,
  ComponentInstallationFailure,
  MissingComponentFailure,
  ProjectManagerUpgradeRequiredFailure
}
import org.enso.projectmanager.versionmanagement.DistributionManagementConfiguration
import org.enso.runtimeversionmanager.components._

trait RuntimeVersionManagerMixin {
  def managers: DistributionManagementConfiguration

  def makeRuntimeVersionManager(
    progressTracker: ActorRef,
    allowMissingComponents: Boolean,
    allowBrokenComponents: Boolean
  ): RuntimeVersionManager =
    managers.makeRuntimeVersionManager(
      new ControllerInterface(
        progressTracker        = progressTracker,
        allowMissingComponents = allowMissingComponents,
        allowBrokenComponents  = allowBrokenComponents
      )
    )

  def makeReadOnlyVersionManager(): RuntimeVersionManager =
    managers.makeRuntimeVersionManager(new NoOpInterface)

  implicit class ErrorRecovery[F[+_, +_]: ErrorChannel, A](
    fa: F[Throwable, A]
  ) {
    def mapRuntimeManagerErrors(
      wrapDefault: Throwable => ProjectServiceFailure
    ): F[ProjectServiceFailure, A] = ErrorChannel[F].mapError(fa) {
      case componentsException: ComponentsException =>
        componentsException match {
          case InstallationError(message, _) =>
            ComponentInstallationFailure(message)
          case BrokenComponentError(message, _) =>
            BrokenComponentFailure(message)
          case ComponentMissingError(message, _) =>
            MissingComponentFailure(message)
          case upgradeRequired: UpgradeRequiredError =>
            ProjectManagerUpgradeRequiredFailure(
              upgradeRequired.expectedVersion
            )
          case _ => wrapDefault(componentsException)
        }
      case other: Throwable =>
        wrapDefault(other)
    }
  }
}

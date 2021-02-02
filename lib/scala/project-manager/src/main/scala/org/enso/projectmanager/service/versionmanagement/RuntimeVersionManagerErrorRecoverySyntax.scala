package org.enso.projectmanager.service.versionmanagement

import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.ProjectServiceFailure._
import org.enso.runtimeversionmanager.components._

object RuntimeVersionManagerErrorRecoverySyntax {
  implicit class ErrorRecovery[F[+_, +_]: ErrorChannel, A](
    fa: F[Throwable, A]
  ) {

    /** Converts relevant [[ComponentsException]] errors into their counterparts
      * in the protocol.
      *
      * @param mapDefault a mapping that should be used for other errors that do
      *                   not have a direct counterpart
      */
    def mapRuntimeManagerErrors(
      mapDefault: Throwable => ProjectServiceFailure
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
          case UninstallationError(message) =>
            ComponentUninstallationFailure(message)
          case _ => mapDefault(componentsException)
        }
      case other: Throwable =>
        mapDefault(other)
    }
  }
}

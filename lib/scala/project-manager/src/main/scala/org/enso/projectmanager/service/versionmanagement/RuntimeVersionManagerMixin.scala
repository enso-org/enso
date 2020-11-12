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
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.components._

/** A helper class that defines methods for creating the
  * [[RuntimeVersionManager]] based on a
  * [[DistributionConfiguration]].
  */
trait RuntimeVersionManagerMixin {

  /** The distribution configuration to use. */
  def distributionConfiguration: DistributionConfiguration

  /** Creates a [[RuntimeVersionManager]] that will send
    * [[ProgressNotification]] to the specified [[ActorRef]] and with the
    * specified settings for handling missing and broken components.
    */
  def makeRuntimeVersionManager(
    progressTracker: ActorRef,
    allowMissingComponents: Boolean,
    allowBrokenComponents: Boolean
  ): RuntimeVersionManager =
    distributionConfiguration.makeRuntimeVersionManager(
      new ControllerInterface(
        progressTracker        = progressTracker,
        allowMissingComponents = allowMissingComponents,
        allowBrokenComponents  = allowBrokenComponents
      )
    )

  /** Creates a simple [[RuntimeVersionManager]] that ignores progress (it can
    * be used when we know that no relevant progress will be reported) and not
    * allowing to install any components.
    *
    * It is useful for simple queries, like listing installed versions.
    */
  def makeReadOnlyVersionManager(): RuntimeVersionManager =
    distributionConfiguration.makeRuntimeVersionManager(new NoOpInterface)

  implicit class ErrorRecovery[F[+_, +_]: ErrorChannel, A](
    fa: F[Throwable, A]
  ) {

    /** Converts relevant [[ComponentsException]] errors into their counterparts
      * in the protocol.
      */
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

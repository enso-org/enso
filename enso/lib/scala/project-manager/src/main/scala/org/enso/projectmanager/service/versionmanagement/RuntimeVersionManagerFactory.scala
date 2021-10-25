package org.enso.projectmanager.service.versionmanagement

import akka.actor.ActorRef
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.components._

/** A helper class that defines methods for creating the
  * [[RuntimeVersionManager]] based on a
  * [[DistributionConfiguration]].
  */
case class RuntimeVersionManagerFactory(
  distributionConfiguration: DistributionConfiguration
) {

  /** Creates a [[RuntimeVersionManager]] that will send
    * [[ProgressNotification]] to the specified [[ActorRef]] and with the
    * specified settings for handling missing and broken components.
    *
    * @param progressTracker the actor that tracks installation progress/lock
    *                        notifications
    * @param allowMissingComponents if set to true, missing components will be
    *                               installed
    * @param allowBrokenComponents if allowMissingComponents and this flag are
    *                              set to true, missing components will be
    *                              installed even if they are marked as broken
    */
  def makeRuntimeVersionManager(
    progressTracker: ActorRef,
    allowMissingComponents: Boolean = false,
    allowBrokenComponents: Boolean  = false
  ): RuntimeVersionManager =
    distributionConfiguration.makeRuntimeVersionManager(
      new ControllerInterface(
        progressTracker        = progressTracker,
        allowMissingComponents = allowMissingComponents,
        allowBrokenComponents  = allowBrokenComponents
      )
    )

  /** Creates a [[RuntimeVersionManager]] that will send
    * [[ProgressNotification]] to the specified [[ActorRef]] and with the
    * specified settings for handling missing and broken components.
    *
    * @param progressTracker the actor that tracks installation progress/lock
    *                        notifications
    * @param missingComponentAction specifies how to handle missing components
    */
  def makeRuntimeVersionManager(
    progressTracker: ActorRef,
    missingComponentAction: MissingComponentAction
  ): RuntimeVersionManager = {
    val (missing, broken) = missingComponentAction match {
      case MissingComponentAction.Fail               => (false, false)
      case MissingComponentAction.Install            => (true, false)
      case MissingComponentAction.ForceInstallBroken => (true, true)
    }
    makeRuntimeVersionManager(
      progressTracker,
      allowMissingComponents = missing,
      allowBrokenComponents  = broken
    )
  }

  /** Creates a simple [[RuntimeVersionManager]] that ignores progress (it can
    * be used when we know that no relevant progress will be reported) and not
    * allowing to install any components.
    *
    * It is useful for simple queries, like listing installed versions.
    */
  def makeReadOnlyVersionManager(): RuntimeVersionManager =
    distributionConfiguration.makeRuntimeVersionManager(new NoOpInterface)
}

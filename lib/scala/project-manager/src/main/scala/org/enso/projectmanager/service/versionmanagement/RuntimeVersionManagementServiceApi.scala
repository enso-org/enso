package org.enso.projectmanager.service.versionmanagement

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.data.EngineVersion
import org.enso.projectmanager.service.ProjectServiceFailure

/** A contract for the Runtime Version Management Service.
  *
  * @tparam F a monadic context
  */
trait RuntimeVersionManagementServiceApi[F[+_, +_]] {

  /** Installs a specified engine version.
    *
    * @param progressTracker an actor that will be sent progress updates
    * @param version engine version to install
    * @param forceInstallBroken if set to true, broken versions will be forcibly
    *                           installed
    */
  def installEngine(
    progressTracker: ActorRef,
    version: SemVer,
    forceInstallBroken: Boolean
  ): F[ProjectServiceFailure, Unit]

  /** Uninstalls a specified engine version.
    *
    * @param progressTracker an actor that will be sent progress updates
    * @param version engine version to uninstall
    */
  def uninstallEngine(
    progressTracker: ActorRef,
    version: SemVer
  ): F[ProjectServiceFailure, Unit]

  /** Lists installed engine versions. */
  def listInstalledEngines(): F[ProjectServiceFailure, Seq[EngineVersion]]

  /** Lists engine versions available in the associated repository. */
  def listAvailableEngines(): F[ProjectServiceFailure, Seq[EngineVersion]]

//  def handleMissingComponents(
//    progressTracker: ActorRef,
//    version: SemVer,
//    missingComponentAction: MissingComponentAction
//  )(implicit flatMap: CovariantFlatMap[F]): F[ProjectServiceFailure, Unit] =
//    missingComponentAction match {
//      case MissingComponentAction.Fail => CovariantFlatMap[F].pure(())
//      case MissingComponentAction.Install =>
//        installEngine(progressTracker, version, forceInstallBroken = false)
//      case MissingComponentAction.ForceInstallBroken =>
//        installEngine(progressTracker, version, forceInstallBroken = true)
//    }
}

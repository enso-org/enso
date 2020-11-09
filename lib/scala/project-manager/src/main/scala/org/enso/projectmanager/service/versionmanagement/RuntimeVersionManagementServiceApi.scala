package org.enso.projectmanager.service.versionmanagement

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.data.EngineVersion
import org.enso.projectmanager.service.ProjectServiceFailure

trait RuntimeVersionManagementServiceApi[F[+_, +_]] {
  def installEngine(
    progressTracker: ActorRef,
    version: SemVer,
    forceInstallBroken: Boolean
  ): F[ProjectServiceFailure, Unit]
  def uninstallEngine(
    progressTracker: ActorRef,
    version: SemVer
  ): F[ProjectServiceFailure, Unit]

  def listInstalledEngines(): F[ProjectServiceFailure, Seq[EngineVersion]]
  def listAvailableEngines(): F[ProjectServiceFailure, Seq[EngineVersion]]
}

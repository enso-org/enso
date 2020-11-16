package org.enso.projectmanager.service

import java.nio.file.Path

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.data.MissingComponentAction

trait ProjectCreationServiceApi[F[+_, +_]] {
  def createProject(
    progressTracker: ActorRef,
    path: Path,
    name: String,
    version: SemVer,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, Unit]
}

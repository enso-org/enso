package org.enso.projectmanager.service

import java.nio.file.Path

import akka.actor.ActorRef
import com.github.zafarkhaja.semver.Version
import org.enso.projectmanager.data.MissingComponentAction

/** An abstraction for creating new project structures under the given location.
  */
trait ProjectCreationServiceApi[F[+_, +_]] {

  /** Creates a project with the provided configuration.
    *
    * @param progressTracker an actor that will be sent notifcation regarding
    *                        progress of installation of any missing components
    *                        or waiting on locks
    * @param path path at which to create the project
    * @param name name of the project
    * @param engineVersion version of the engine this project is meant for
    * @param projectTemplate the name of the project template
    * @param missingComponentAction specifies how to handle missing components
    */
  def createProject(
    progressTracker: ActorRef,
    path: Path,
    name: String,
    engineVersion: Version,
    projectTemplate: Option[String],
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, Unit]
}

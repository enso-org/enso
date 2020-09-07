package org.enso.projectmanager.service

import java.util.UUID

import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.infrastructure.log.Logging
import org.enso.projectmanager.infrastructure.repository.{
  ProjectRepository,
  ProjectRepositoryFailure
}
import org.enso.projectmanager.infrastructure.shutdown.ShutdownHook

/**
  * A hook responsible for moving a project to the target dir.
  *
  * @param projectId a project id
  * @param newName a new project name
  * @param repo a project repository
  * @param log a logging facility
  */
class MoveProjectDirCmd[F[+_, +_]: CovariantFlatMap: ErrorChannel](
  projectId: UUID,
  newName: String,
  repo: ProjectRepository[F],
  log: Logging[F]
) extends ShutdownHook[F] {

  /** @inheritdoc */
  override def execute(): F[Nothing, Unit] = {
    def go() =
      for {
        _   <- log.debug(s"Moving project ${projectId} to $newName")
        dir <- repo.moveProjectToTargetDir(projectId, newName)
        _   <- log.info(s"Project $projectId moved to $dir")
      } yield ()

    go().fallbackTo(logError)
  }

  private def logError(failure: ProjectRepositoryFailure): F[Nothing, Unit] = {
    log.error(
      s"An error occurred during moving project $projectId [$failure]"
    )
  }

}

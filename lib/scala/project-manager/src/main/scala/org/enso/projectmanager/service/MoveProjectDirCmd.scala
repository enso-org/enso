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

/** A hook responsible for moving a project to the target dir.
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
        _   <- log.debug("Moving project [{}] to [{}].", projectId, newName)
        dir <- repo.moveProjectToTargetDir(projectId, newName)
        _   <- log.info("Project [{}] moved to [{}].", projectId, dir)
      } yield ()

    go().fallbackTo(logError)
  }

  private def logError(failure: ProjectRepositoryFailure): F[Nothing, Unit] = {
    log.error(
      "An error occurred during moving project {} [{}].",
      projectId,
      failure
    )
  }

}

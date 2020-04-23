package org.enso.projectmanager.service

import java.util.UUID

import cats.MonadError
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.ErrorChannel
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.data.{
  LanguageServerSockets,
  ProjectMetadata,
  Socket
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerService
import org.enso.projectmanager.infrastructure.log.Logging
import org.enso.projectmanager.infrastructure.random.Generator
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure.{
  CannotLoadIndex,
  InconsistentStorage,
  ProjectNotFoundInIndex,
  StorageFailure
}
import org.enso.projectmanager.infrastructure.repository.{
  ProjectRepository,
  ProjectRepositoryFailure
}
import org.enso.projectmanager.infrastructure.time.Clock
import org.enso.projectmanager.model.Project
import org.enso.projectmanager.model.ProjectKind.UserProject
import org.enso.projectmanager.service.ProjectServiceFailure._
import org.enso.projectmanager.service.ValidationFailure.{
  EmptyName,
  NameContainsForbiddenCharacter
}

/**
  * Implementation of business logic for project management.
  *
  * @param validator a project validator
  * @param repo a project repository
  * @param log a logging facility
  * @param clock a clock
  * @param gen a random generator
  */
class ProjectService[F[+_, +_]: ErrorChannel: CovariantFlatMap](
  validator: ProjectValidator[F],
  repo: ProjectRepository[F],
  log: Logging[F],
  clock: Clock[F],
  gen: Generator[F],
  languageServerService: LanguageServerService[F]
)(implicit E: MonadError[F[ProjectServiceFailure, *], ProjectServiceFailure])
    extends ProjectServiceApi[F] {

  import E._

  /** @inheritdoc **/
  override def createUserProject(
    name: String
  ): F[ProjectServiceFailure, UUID] = {
    // format: off
    for {
      _            <- log.debug(s"Creating project $name.")
      _            <- validateName(name)
      _            <- validateExists(name)
      creationTime <- clock.nowInUtc()
      projectId    <- gen.randomUUID()
      project       = Project(projectId, name, UserProject, creationTime)
      _            <- repo.save(project).mapError(toServiceFailure)
      _            <- log.info(s"Project $project created.")
    } yield projectId
    // format: on
  }

  /** @inheritdoc **/
  override def deleteUserProject(
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] =
    log.debug(s"Deleting project $projectId.") *>
    ensureProjectIsNotRunning(projectId) *>
    repo.delete(projectId).mapError(toServiceFailure) *>
    log.info(s"Project $projectId deleted.")

  private def ensureProjectIsNotRunning(
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] =
    languageServerService
      .isRunning(projectId)
      .mapError(_ => ProjectOperationTimeout)
      .flatMap {
        case false => CovariantFlatMap[F].pure(())
        case true  => ErrorChannel[F].fail(CannotRemoveOpenProject)
      }

  /** @inheritdoc **/
  override def openProject(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, LanguageServerSockets] = {
    // format: off
    for {
      _        <- log.debug(s"Opening project $projectId")
      project  <- getUserProject(projectId)
      openTime <- clock.nowInUtc()
      updated   = project.copy(lastOpened = Some(openTime))
      _        <- repo.save(updated).mapError(toServiceFailure)
      sockets  <- startServer(clientId, updated)
    } yield sockets
    // format: on
  }

  private def startServer(
    clientId: UUID,
    project: Project
  ): F[ProjectServiceFailure, LanguageServerSockets] =
    languageServerService
      .start(clientId, project)
      .mapError {
        case ServerBootTimedOut =>
          ProjectOpenFailed("Language server boot timed out")

        case ServerBootFailed(th) =>
          ProjectOpenFailed(
            s"Language server boot failed: ${th.getMessage}"
          )
      }

  /** @inheritdoc **/
  override def closeProject(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] = {
    log.debug(s"Closing project $projectId") *>
    languageServerService.stop(clientId, projectId).mapError {
      case FailureDuringStoppage(th)    => ProjectCloseFailed(th.getMessage)
      case ServerNotRunning             => ProjectNotOpen
      case CannotDisconnectOtherClients => ProjectOpenByOtherPeers
    }
  }

  /** @inheritdoc **/
  override def listRecentProjects(
    size: Int
  ): F[ProjectServiceFailure, List[ProjectMetadata]] =
    repo
      .getAll()
      .map(_.sorted(RecentlyUsedProjectsOrdering).take(size))
      .map(_.map(toProjectMetadata))
      .mapError(toServiceFailure)

  private def toProjectMetadata(project: Project): ProjectMetadata =
    ProjectMetadata(
      name       = project.name,
      id         = project.id,
      lastOpened = project.lastOpened
    )

  private def getUserProject(
    projectId: UUID
  ): F[ProjectServiceFailure, Project] =
    repo
      .findById(projectId)
      .mapError(toServiceFailure)
      .flatMap {
        case None          => ErrorChannel[F].fail(ProjectNotFound)
        case Some(project) => CovariantFlatMap[F].pure(project)
      }

  private def validateExists(
    name: String
  ): F[ProjectServiceFailure, Unit] =
    repo
      .exists(name)
      .mapError(toServiceFailure)
      .flatMap { exists =>
        if (exists) raiseError(ProjectExists)
        else unit
      }

  private val toServiceFailure
    : ProjectRepositoryFailure => ProjectServiceFailure = {
    case CannotLoadIndex(msg) =>
      DataStoreFailure(s"Cannot load project index [$msg]")
    case StorageFailure(msg) =>
      DataStoreFailure(s"Storage failure [$msg]")
    case ProjectNotFoundInIndex =>
      ProjectNotFound
    case InconsistentStorage(msg) =>
      DataStoreFailure(s"Project repository inconsistency detected [$msg]")
  }

  private def validateName(
    name: String
  ): F[ProjectServiceFailure, Unit] =
    validator
      .validateName(name)
      .mapError {
        case EmptyName =>
          ProjectServiceFailure.ValidationFailure(
            "Cannot create project with empty name"
          )
        case NameContainsForbiddenCharacter(chars) =>
          ProjectServiceFailure.ValidationFailure(
            s"Project name contains forbidden characters: ${chars.mkString(",")}"
          )
      }

}

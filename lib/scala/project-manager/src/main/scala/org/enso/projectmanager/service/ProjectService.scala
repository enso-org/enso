package org.enso.projectmanager.service

import java.util.UUID

import cats.MonadError
import org.enso.pkg.{EnsoVersion, PackageManager}
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.data.{
  LanguageServerSockets,
  MissingComponentAction,
  ProjectMetadata
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerGateway
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

/** Implementation of business logic for project management.
  *
  * @param validator a project validator
  * @param repo a project repository
  * @param log a logging facility
  * @param clock a clock
  * @param gen a random generator
  */
class ProjectService[F[+_, +_]: ErrorChannel: CovariantFlatMap: Sync](
  validator: ProjectValidator[F],
  repo: ProjectRepository[F],
  log: Logging[F],
  clock: Clock[F],
  gen: Generator[F],
  languageServerGateway: LanguageServerGateway[F]
)(implicit E: MonadError[F[ProjectServiceFailure, *], ProjectServiceFailure])
    extends ProjectServiceApi[F] {

  import E._

  /** @inheritdoc */
  override def createUserProject(
    name: String,
    version: EnsoVersion,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, UUID] = {
    // TODO [RW] new component handling
    val _ = (version, missingComponentAction)
    // format: off
    for {
      projectId    <- gen.randomUUID()
      _            <- log.debug(s"Creating project $name $projectId.")
      _            <- validateName(name)
      _            <- checkIfNameExists(name)
      creationTime <- clock.nowInUtc()
      project       = Project(projectId, name, UserProject, creationTime)
      _            <- repo.create(project).mapError(toServiceFailure)
      _            <- log.info(s"Project $project created.")
    } yield projectId
    // format: on
  }

  /** @inheritdoc */
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
    isServerRunning(projectId)
      .flatMap {
        case false => CovariantFlatMap[F].pure(())
        case true  => ErrorChannel[F].fail(CannotRemoveOpenProject)
      }

  private def isServerRunning(
    projectId: UUID
  ): F[ProjectServiceFailure, Boolean] =
    languageServerGateway
      .isRunning(projectId)
      .mapError(_ => ProjectOperationTimeout)

  /** @inheritdoc */
  override def renameProject(
    projectId: UUID,
    name: String
  ): F[ProjectServiceFailure, Unit] = {
    for {
      _          <- log.debug(s"Renaming project $projectId to $name.")
      _          <- validateName(name)
      _          <- checkIfProjectExists(projectId)
      _          <- checkIfNameExists(name)
      oldPackage <- repo.getPackageName(projectId).mapError(toServiceFailure)
      _          <- repo.rename(projectId, name).mapError(toServiceFailure)
      _          <- renameProjectDirOrRegisterShutdownHook(projectId, name)
      newPackage = PackageManager.Default.normalizeName(name)
      _ <- refactorProjectName(projectId, oldPackage, newPackage)
      _ <- log.info(s"Project $projectId renamed.")
    } yield ()
  }

  private def renameProjectDirOrRegisterShutdownHook(
    projectId: UUID,
    newName: String
  ): F[ProjectServiceFailure, Unit] = {
    val cmd = new MoveProjectDirCmd[F](projectId, newName, repo, log)
    CovariantFlatMap[F]
      .ifM(isServerRunning(projectId))(
        ifTrue  = languageServerGateway.registerShutdownHook(projectId, cmd),
        ifFalse = cmd.execute()
      )
  }

  private def refactorProjectName(
    projectId: UUID,
    oldPackage: String,
    newPackage: String
  ): F[ProjectServiceFailure, Unit] =
    languageServerGateway
      .renameProject(
        projectId,
        oldPackage,
        newPackage
      )
      .recover { case ProjectNotOpened =>
        ()
      }
      .mapError {
        case ProjectNotOpened => ProjectNotOpen //impossible
        case RenameTimeout    => ProjectOperationTimeout
        case CannotConnectToServer =>
          LanguageServerFailure("Cannot connect to the language server")

        case RenameFailure(code, msg) =>
          LanguageServerFailure(
            s"Failure during renaming [code: $code message: $msg]"
          )

        case ServerUnresponsive =>
          LanguageServerFailure("The language server is unresponsive")
      }

  private def checkIfProjectExists(
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] =
    repo
      .findById(projectId)
      .mapError(toServiceFailure)
      .flatMap {
        case None    => ErrorChannel[F].fail(ProjectNotFound)
        case Some(_) => CovariantFlatMap[F].pure(())
      }

  /** @inheritdoc */
  override def openProject(
    clientId: UUID,
    projectId: UUID,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, LanguageServerSockets] = {
    // TODO [RW] new component handling
    val _ = missingComponentAction
    // format: off
    for {
      _        <- log.debug(s"Opening project $projectId")
      project  <- getUserProject(projectId)
      openTime <- clock.nowInUtc()
      updated   = project.copy(lastOpened = Some(openTime))
      _        <- repo.update(updated).mapError(toServiceFailure)
      sockets  <- startServer(clientId, updated)
    } yield sockets
    // format: on
  }

  private def startServer(
    clientId: UUID,
    project: Project
  ): F[ProjectServiceFailure, LanguageServerSockets] =
    languageServerGateway
      .start(clientId, project)
      .mapError {
        case PreviousInstanceNotShutDown =>
          ProjectOpenFailed(
            "The previous instance of the Language Server hasn't been shut " +
            "down yet."
          )

        case ServerBootTimedOut =>
          ProjectOpenFailed("Language server boot timed out")

        case ServerBootFailed(th) =>
          ProjectOpenFailed(
            s"Language server boot failed: ${th.getMessage}"
          )
      }

  /** @inheritdoc */
  override def closeProject(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] = {
    log.debug(s"Closing project $projectId") *>
    languageServerGateway.stop(clientId, projectId).mapError {
      case ServerShutdownTimedOut =>
        ProjectCloseFailed("Server shutdown timed out")

      case FailureDuringShutdown(th)    => ProjectCloseFailed(th.getMessage)
      case ServerNotRunning             => ProjectNotOpen
      case CannotDisconnectOtherClients => ProjectOpenByOtherPeers
    }
  }

  /** @inheritdoc */
  override def listProjects(
    maybeSize: Option[Int]
  ): F[ProjectServiceFailure, List[ProjectMetadata]] =
    repo
      .getAll()
      .map(
        _.sorted(RecentlyUsedProjectsOrdering)
          .take(maybeSize.getOrElse(Int.MaxValue))
      )
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

  private def checkIfNameExists(
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
            "Project name cannot be empty"
          )
        case NameContainsForbiddenCharacter(chars) =>
          ProjectServiceFailure.ValidationFailure(
            s"Project name contains forbidden characters: ${chars.mkString(",")}"
          )
      }

}

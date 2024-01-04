package org.enso.projectmanager.service

import akka.actor.ActorRef
import cats.MonadError
import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.editions.DefaultEdition
import org.enso.pkg.Config
import org.enso.pkg.validation.NameValidation
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.core.CovariantFlatMap
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Semaphore, Sync}
import org.enso.projectmanager.data.{
  LanguageServerStatus,
  MissingComponentAction,
  ProjectMetadata,
  RunningLanguageServerInfo
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerGateway
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol._
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
import org.enso.projectmanager.service.config.GlobalConfigServiceApi
import org.enso.projectmanager.service.validation.ProjectNameValidator
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerErrorRecoverySyntax._
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerFactory
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.CurrentVersion

import java.util.UUID

/** Implementation of business logic for project management.
  *
  * @param validator a project validator
  * @param repo a project repository
  * @param projectCreationService a service for creating projects
  * @param configurationService a service for managing configuration
  * @param log a logging facility
  * @param clock a clock
  * @param gen a random generator
  */
class ProjectService[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap
](
  validator: ProjectNameValidator[F],
  repo: ProjectRepository[F],
  projectCreationService: ProjectCreationServiceApi[F],
  configurationService: GlobalConfigServiceApi[F],
  log: Logging[F],
  clock: Clock[F],
  gen: Generator[F],
  languageServerGateway: LanguageServerGateway[F],
  distributionConfiguration: DistributionConfiguration
)(implicit E: MonadError[F[ProjectServiceFailure, *], ProjectServiceFailure])
    extends ProjectServiceApi[F] {

  private lazy val logger = Logger[ProjectService[F]]

  private val listProjectsLock: Semaphore[F] = Semaphore.unsafeMake(1)

  import E._

  /** @inheritdoc */
  override def createUserProject(
    progressTracker: ActorRef,
    projectName: String,
    engineVersion: SemVer,
    projectTemplate: Option[String],
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, Project] = for {
    projectId <- gen.randomUUID()
    _ <- log.debug(
      "Creating project [{}, {}, {}].",
      projectName,
      projectId,
      projectTemplate
    )
    name         <- getNameForNewProject(projectName, projectTemplate)
    _            <- log.info("Created project with actual name [{}].", name)
    _            <- validateProjectName(name)
    _            <- checkIfNameExists(name)
    creationTime <- clock.nowInUtc()
    moduleName = NameValidation.normalizeName(name)
    path <- repo.findPathForNewProject(moduleName).mapError(toServiceFailure)
    project = Project(
      id        = projectId,
      name      = name,
      module    = moduleName,
      namespace = Config.defaultNamespace,
      kind      = UserProject,
      created   = creationTime,
      edition   = None,
      path      = path.toFile
    )
    _ <- log.debug(
      "Found a path [{}] for a new project [{}, {}].",
      path,
      name,
      projectId
    )
    _ <- projectCreationService.createProject(
      progressTracker,
      path,
      name,
      engineVersion,
      projectTemplate,
      missingComponentAction
    )
    _ <- log.debug(
      "Project [{}] structure created with [{}, {}, {}, {}].",
      projectId,
      path,
      name,
      moduleName
    )
    _ <- repo.update(project).mapError(toServiceFailure)
    _ <- log.debug("Project [{}] updated in repository [{}].", projectId, repo)
    _ <- log.info("Project created [{}].", project)
  } yield project

  /** @inheritdoc */
  override def deleteUserProject(
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] =
    log.debug("Deleting project [{}].", projectId) *>
    ensureProjectIsNotRunning(projectId) *>
    repo.delete(projectId).mapError(toServiceFailure) *>
    log.info("Project deleted [{}].", projectId)

  private def ensureProjectIsNotRunning(
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] =
    isServerRunning(projectId)
      .flatMap {
        case (false, _)    => CovariantFlatMap[F].pure(())
        case (true, true)  => ErrorChannel[F].fail(CannotRemoveClosingProject)
        case (true, false) => ErrorChannel[F].fail(CannotRemoveOpenProject)
      }

  private def isServerRunning(
    projectId: UUID
  ): F[ProjectServiceFailure, (Boolean, Boolean)] =
    languageServerGateway
      .isRunning(projectId)
      .mapError(_ => ProjectOperationTimeout)

  /** @inheritdoc */
  override def renameProject(
    projectId: UUID,
    newName: String
  ): F[ProjectServiceFailure, Unit] = {
    for {
      _          <- log.debug("Renaming project [{}] to [{}].", projectId, newName)
      _          <- validateProjectName(newName)
      _          <- checkIfProjectExists(projectId)
      _          <- checkIfNameExists(newName)
      oldPackage <- repo.getPackageName(projectId).mapError(toServiceFailure)
      namespace <- repo
        .getPackageNamespace(projectId)
        .mapError(toServiceFailure)
      newPackage = NameValidation.normalizeName(newName)
      _ <- repo.rename(projectId, newName).mapError(toServiceFailure)
      _ <- renameProjectDirOrRegisterShutdownHook(projectId, newPackage)
      _ <- refactorProjectName(projectId, namespace, oldPackage, newPackage)
      _ <- log.info("Project renamed [{}].", projectId)
    } yield ()
  }

  private def renameProjectDirOrRegisterShutdownHook(
    projectId: UUID,
    newName: String
  ): F[ProjectServiceFailure, Unit] = {
    val cmd = new MoveProjectDirCmd[F](projectId, newName, repo, log)
    CovariantFlatMap[F]
      .ifM(isServerRunning(projectId).map(_._1))(
        ifTrue = for {
          _ <- log.debug(
            "Registering shutdown hook to rename the project [{}] " +
            "with a new name [{}].",
            projectId,
            newName
          )
          _ <- languageServerGateway.registerShutdownHook(projectId, cmd)
        } yield (),
        ifFalse = for {
          _ <- log.debug(
            "Running a command to rename the project [{}] " +
            "with a new name [{}].",
            projectId,
            newName
          )
          _ <- cmd.execute()
        } yield ()
      )
  }

  private def refactorProjectName(
    projectId: UUID,
    namespace: String,
    oldPackage: String,
    newPackage: String
  ): F[ProjectServiceFailure, Unit] =
    languageServerGateway
      .renameProject(
        projectId,
        namespace,
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
          LanguageServerFailure("Cannot connect to the language server.")

        case RenameFailure(code, msg) =>
          LanguageServerFailure(
            s"Failure during renaming [code: $code message: $msg]."
          )

        case ServerUnresponsive =>
          LanguageServerFailure("The language server is unresponsive.")
      }
      .flatMap { _ =>
        log.debug(
          "Language Server replied to the project [{}] rename command from {} to {}.",
          projectId,
          oldPackage,
          newPackage
        )
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
      .flatMap { _ =>
        log.debug(
          "Checked if the project [{}] exists in repo [{}].",
          projectId,
          repo
        )
      }

  /** @inheritdoc */
  override def openProject(
    progressTracker: ActorRef,
    clientId: UUID,
    projectId: UUID,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, RunningLanguageServerInfo] = {
    for {
      _        <- log.debug(s"Opening project [{}].", projectId)
      project  <- getUserProject(projectId)
      openTime <- clock.nowInUtc()
      updated = project.copy(lastOpened = Some(openTime))
      _ <- repo.update(updated).mapError(toServiceFailure)
      projectWithDefaultEdition =
        updated.copy(edition = Some(DefaultEdition.getDefaultEdition))
      sockets <- startServer(
        progressTracker,
        clientId,
        projectWithDefaultEdition,
        missingComponentAction
      )
    } yield sockets
  }

  private def preinstallEngine(
    progressTracker: ActorRef,
    version: SemVer,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, Unit] =
    Sync[F]
      .blockingOp {
        val runtimeVersionManager =
          RuntimeVersionManagerFactory(distributionConfiguration)
            .makeRuntimeVersionManager(progressTracker, missingComponentAction)
        val engine = runtimeVersionManager.findOrInstallEngine(version)
        runtimeVersionManager.findOrInstallGraalRuntime(engine)
        ()
      }
      .mapRuntimeManagerErrors(th =>
        ProjectOpenFailed(
          s"Cannot install the required engine. $th"
        )
      )

  private def startServer(
    progressTracker: ActorRef,
    clientId: UUID,
    project: Project,
    missingComponentAction: MissingComponentAction
  ): F[ProjectServiceFailure, RunningLanguageServerInfo] = for {
    _       <- log.debug("Preparing to start the Language Server for [{}].", project)
    version <- resolveProjectVersion(project)
    _       <- preinstallEngine(progressTracker, version, missingComponentAction)
    sockets <- languageServerGateway
      .start(progressTracker, clientId, project, version)
      .mapError {
        case PreviousInstanceNotShutDown =>
          ProjectOpenFailed(
            "The previous instance of the Language Server hasn't been shut down yet."
          )

        case ServerBootTimedOut =>
          ProjectOpenFailed("Language server boot timed out.")

        case ServerBootFailed(th) =>
          ProjectOpenFailed(s"Language server boot failed. ${th.getMessage}")
      }
  } yield RunningLanguageServerInfo(
    version,
    sockets,
    project.name,
    project.module,
    project.namespace
  )

  /** @inheritdoc */
  override def closeProject(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, Unit] = {
    log.debug(s"Closing project [{}].", projectId) *>
    languageServerGateway.stop(clientId, projectId).mapError {
      case ServerShutdownTimedOut =>
        ProjectCloseFailed("Server shutdown timed out.")

      case FailureDuringShutdown(th)    => ProjectCloseFailed(th.getMessage)
      case ServerNotRunning             => ProjectNotOpen
      case CannotDisconnectOtherClients => ProjectOpenByOtherPeers
    }
  }

  /** @inheritdoc */
  override def listProjects(
    maybeSize: Option[Int]
  ): F[ProjectServiceFailure, List[ProjectMetadata]] =
    listProjectsLock
      .withPermit(repo.getAll())
      .map(
        _.sorted(RecentlyUsedProjectsOrdering)
          .take(maybeSize.getOrElse(Int.MaxValue))
      )
      .mapError(toServiceFailure)
      .map(_.map(toProjectMetadata))

  private def toProjectMetadata(project: Project): ProjectMetadata =
    ProjectMetadata(
      name       = project.name,
      namespace  = project.namespace,
      id         = project.id,
      created    = project.created,
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
      .flatMap { project =>
        log
          .debug("Found project [{}] in [{}].", projectId, repo)
          .map(_ => project)
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
      .flatMap { _ =>
        log.debug(
          "Checked if the project name [{}] exists in [{}].",
          name,
          repo
        )
      }

  private val toServiceFailure
    : ProjectRepositoryFailure => ProjectServiceFailure = {
    case CannotLoadIndex(msg) =>
      DataStoreFailure(s"Cannot load project index [$msg].")
    case StorageFailure(msg) =>
      DataStoreFailure(s"Storage failure [$msg].")
    case ProjectNotFoundInIndex =>
      ProjectNotFound
    case InconsistentStorage(msg) =>
      DataStoreFailure(s"Project repository inconsistency detected [$msg].")
  }

  private def validateProjectName(
    name: String
  ): F[ProjectServiceFailure, Unit] =
    validator
      .validate(name)
      .mapError { case ProjectNameValidator.ValidationFailure.EmptyName =>
        ProjectServiceFailure.ValidationFailure(
          "Project name cannot be empty."
        )
      }
      .flatMap { _ =>
        log.debug("Project name [{}] validated by [{}].", name, validator)
      }

  private def resolveProjectVersion(
    project: Project
  ): F[ProjectServiceFailure, SemVer] =
    if (project.edition.contains(DefaultEdition.getDefaultEdition)) {
      CovariantFlatMap[F].pure(CurrentVersion.version)
    } else {
      Sync[F]
        .blockingOp {
          // TODO [RW] at some point we will need to use the configuration service to get the actual default version, see #1864
          val _ = configurationService

          val edition =
            project.edition.getOrElse(DefaultEdition.getDefaultEdition)

          distributionConfiguration.editionManager
            .resolveEngineVersion(edition)
            .orElse {
              logger.warn(
                s"Could not resolve engine version for [{}]. Falling back to [{}].",
                edition,
                DefaultEdition.getDefaultEdition
              )
              distributionConfiguration.editionManager
                .resolveEngineVersion(DefaultEdition.getDefaultEdition)
            }
            .get
        }
        .mapError { error =>
          ProjectServiceFailure.GlobalConfigurationAccessFailure(
            s"Could not resolve project engine version: ${error.getMessage}"
          )
        }
    }

  private def getNameForNewProject(
    projectName: String,
    projectTemplate: Option[String]
  ): F[ProjectServiceFailure, String] = {
    def mkName(name: String, suffix: Int): String =
      s"${name}_${suffix}"
    def findAvailableName(
      projectName: String,
      suffix: Int
    ): F[ProjectRepositoryFailure, String] = {
      val newName = mkName(projectName, suffix)
      CovariantFlatMap[F].ifM(repo.exists(newName))(
        ifTrue  = findAvailableName(projectName, suffix + 1),
        ifFalse = CovariantFlatMap[F].pure(newName)
      )
    }

    projectTemplate match {
      case Some(_) =>
        CovariantFlatMap[F]
          .ifM(repo.exists(projectName))(
            ifTrue  = findAvailableName(projectName, 1),
            ifFalse = CovariantFlatMap[F].pure(projectName)
          )
          .mapError(toServiceFailure)
      case None =>
        CovariantFlatMap[F].pure(projectName)
    }

  }

  /** Retrieve project info.
    *
    * @param clientId  the requester id
    * @param projectId the project id
    * @return either failure or [[LanguageServerStatus]] representing success
    */
  override def getProjectStatus(
    clientId: UUID,
    projectId: UUID
  ): F[ProjectServiceFailure, LanguageServerStatus] = {
    log.debug(s"Retrieving the state of project [{}].", projectId)
    languageServerGateway
      .isRunning(projectId)
      .map(e => LanguageServerStatus(e._1, e._2))
      .mapError(_ => LanguageServerFailure("failed to retrieve project state"))
  }
}

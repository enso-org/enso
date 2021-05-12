package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.nio.file.Path
import java.nio.file.attribute.FileTime
import java.util.UUID

import org.enso.pkg.{Package, PackageManager}
import org.enso.projectmanager.boot.configuration.StorageConfig
import org.enso.projectmanager.control.core.{
  Applicative,
  CovariantFlatMap,
  Traverse
}
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.FileSystem
import org.enso.projectmanager.infrastructure.file.FileSystemFailure.{
  FileNotFound,
  NotDirectory
}
import org.enso.projectmanager.infrastructure.random.Generator
import org.enso.projectmanager.infrastructure.repository.ProjectRepositoryFailure.{
  InconsistentStorage,
  ProjectNotFoundInIndex,
  StorageFailure
}
import org.enso.projectmanager.infrastructure.time.Clock
import org.enso.projectmanager.model.{Project, ProjectMetadata}

/** File based implementation of the project repository.
  *
  * @param storageConfig a storage config
  * @param clock a clock
  * @param fileSystem a file system abstraction
  * @param gen a random generator
  */
class ProjectFileRepository[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap: Applicative
](
  storageConfig: StorageConfig,
  clock: Clock[F],
  fileSystem: FileSystem[F],
  gen: Generator[F]
) extends ProjectRepository[F] {

  /** @inheritdoc */
  override def exists(
    name: String
  ): F[ProjectRepositoryFailure, Boolean] =
    getAll().map(_.exists(_.name == name))

  /** @inheritdoc */
  override def find(
    predicate: Project => Boolean
  ): F[ProjectRepositoryFailure, List[Project]] =
    getAll().map(_.filter(predicate))

  /** @inheritdoc */
  override def getAll(): F[ProjectRepositoryFailure, List[Project]] = {
    fileSystem
      .list(storageConfig.userProjectsPath)
      .map(_.filter(_.isDirectory))
      .recover { case FileNotFound | NotDirectory =>
        Nil
      }
      .mapError(th => StorageFailure(th.toString))
      .flatMap { dirs =>
        Traverse[List].traverse(dirs)(tryLoadProject).map(_.flatten)
      }
      .flatMap(resolveClashingIds)
  }

  /** @inheritdoc */
  override def findById(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Option[Project]] =
    getAll().map(_.find(_.id == projectId))

  /** @inheritdoc */
  override def findPathForNewProject(
    project: Project
  ): F[ProjectRepositoryFailure, Path] =
    findTargetPath(project).map(_.toPath)

  private def tryLoadProject(
    directory: File
  ): F[ProjectRepositoryFailure, Option[Project]] = {
    def noop[A]: F[ProjectRepositoryFailure, Option[A]] =
      Applicative[F].pure(None)
    for {
      pkgOpt <- loadPackage(directory)
      metaOpt <- pkgOpt.fold(noop[ProjectMetadata])(_ =>
        loadMetadata(directory)
      )
      directoryCreationTime <- pkgOpt.fold(noop[FileTime])(
        getDirectoryCreationTime(_)
          .map(Some(_))
          .recoverWith(_ => noop)
      )
    } yield for {
      pkg  <- pkgOpt
      meta <- metaOpt
    } yield {
      Project(
        id                    = meta.id,
        name                  = pkg.name,
        kind                  = meta.kind,
        created               = meta.created,
        engineVersion         = pkg.config.ensoVersion,
        lastOpened            = meta.lastOpened,
        path                  = Some(directory.toString),
        directoryCreationTime = directoryCreationTime
      )
    }
  }

  private def loadMetadata(
    directory: File
  ): F[ProjectRepositoryFailure, Option[ProjectMetadata]] =
    metadataStorage(directory)
      .load()
      .map(Some(_))
      .mapError(_.fold(convertFileStorageFailure))

  /** @inheritdoc */
  override def rename(
    projectId: UUID,
    name: String
  ): F[ProjectRepositoryFailure, Unit] =
    findById(projectId).flatMap {
      case Some(project) =>
        project.path match {
          case Some(directory) =>
            renamePackage(new File(directory), name)
          case None =>
            ErrorChannel[F].fail(ProjectNotFoundInIndex)
        }
      case None =>
        ErrorChannel[F].fail(ProjectNotFoundInIndex)
    }

  private def getProject(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Project] =
    findById(projectId)
      .flatMap {
        case None          => ErrorChannel[F].fail(ProjectNotFoundInIndex)
        case Some(project) => CovariantFlatMap[F].pure(project)
      }

  /** @inheritdoc */
  def getPackageName(projectId: UUID): F[ProjectRepositoryFailure, String] = {
    for {
      project        <- getProject(projectId)
      projectPackage <- getPackage(new File(project.path.get))
    } yield projectPackage.config.name
  }

  private def loadPackage(
    projectPath: File
  ): F[ProjectRepositoryFailure, Option[Package[File]]] =
    Sync[F]
      .blockingOp { PackageManager.Default.fromDirectory(projectPath) }
      .mapError(th => StorageFailure(th.toString))

  private def getDirectoryCreationTime(
    pkg: Package[File]
  ): F[ProjectRepositoryFailure, FileTime] =
    Sync[F]
      .blockingOp(pkg.fileSystem.getCreationTime(pkg.root))
      .mapError(th => StorageFailure(th.toString))

  private def getPackage(
    projectPath: File
  ): F[ProjectRepositoryFailure, Package[File]] =
    loadPackage(projectPath)
      .flatMap {
        case None =>
          ErrorChannel[F].fail(
            InconsistentStorage(s"Cannot find package.yaml at $projectPath")
          )
        case Some(projectPackage) => CovariantFlatMap[F].pure(projectPackage)
      }

  private def renamePackage(
    projectPath: File,
    newName: String
  ): F[ProjectRepositoryFailure, Unit] =
    getPackage(projectPath)
      .flatMap { projectPackage =>
        Sync[F]
          .blockingOp { projectPackage.rename(newName) }
          .map(_ => ())
          .mapError(th => StorageFailure(th.toString))
      }

  /** @inheritdoc */
  def update(project: Project): F[ProjectRepositoryFailure, Unit] =
    project.path match {
      case Some(path) =>
        metadataStorage(new File(path))
          .persist(
            ProjectMetadata(
              id         = project.id,
              kind       = project.kind,
              created    = project.created,
              lastOpened = project.lastOpened
            )
          )
          .mapError(th => StorageFailure(th.toString))
      case None =>
        ErrorChannel[F].fail(ProjectNotFoundInIndex)
    }

  /** @inheritdoc */
  override def delete(projectId: UUID): F[ProjectRepositoryFailure, Unit] = {
    findById(projectId)
      .flatMap {
        case Some(project) =>
          project.path match {
            case Some(directory) =>
              fileSystem
                .removeDir(new File(directory))
                .mapError(th => StorageFailure(th.toString))
            case None =>
              ErrorChannel[F].fail(ProjectNotFoundInIndex)
          }
        case None =>
          ErrorChannel[F].fail(ProjectNotFoundInIndex)
      }
  }

  /** @inheritdoc */
  override def moveProjectToTargetDir(
    projectId: UUID,
    newName: String
  ): F[ProjectRepositoryFailure, File] = {
    def move(project: Project) =
      for {
        targetPath <- findTargetPath(project.copy(name = newName))
        _          <- moveProjectDir(project, targetPath)
      } yield targetPath

    for {
      project <- getProject(projectId)
      primaryPath = new File(storageConfig.userProjectsPath, newName)
      finalPath <-
        if (isLocationOk(project.path.get, primaryPath.toString)) {
          CovariantFlatMap[F].pure(primaryPath)
        } else {
          move(project)
        }
    } yield finalPath
  }

  private def isLocationOk(
    currentPath: String,
    primaryPath: String
  ): Boolean = {
    if (currentPath.startsWith(primaryPath)) {
      val suffixPattern = "_\\d+"
      val suffix        = currentPath.substring(primaryPath.length, currentPath.length)
      suffix.matches(suffixPattern)
    } else {
      false
    }
  }

  private def moveProjectDir(project: Project, targetPath: File) = {
    fileSystem
      .move(new File(project.path.get), targetPath)
      .mapError[ProjectRepositoryFailure](failure =>
        StorageFailure(failure.toString)
      )
  }

  private def findTargetPath(
    project: Project
  ): F[ProjectRepositoryFailure, File] =
    CovariantFlatMap[F]
      .tailRecM[ProjectRepositoryFailure, Int, File](0) { number =>
        val path =
          new File(
            storageConfig.userProjectsPath,
            project.name + genSuffix(number)
          )
        fileSystem
          .exists(path)
          .mapError[ProjectRepositoryFailure](failure =>
            StorageFailure(failure.toString)
          )
          .flatMap { fileExists =>
            if (fileExists) {
              CovariantFlatMap[F].pure(Left(number + 1))
            } else {
              CovariantFlatMap[F].pure(Right(path))
            }
          }
      }

  private def genSuffix(number: Int) =
    if (number == 0) ""
    else s"_$number"

  private def metadataStorage(projectPath: File): MetadataFileStorage[F] =
    new MetadataFileStorage[F](
      projectPath,
      storageConfig,
      clock,
      fileSystem,
      gen
    )

  /** Resolve id clashes and return a list of projects with unique identifiers
    * by assigning random ids to clashing projects.
    *
    * @param projects the list of projects
    * @return return the list of projects with unique ids
    */
  private def resolveClashingIds(
    projects: List[Project]
  ): F[ProjectRepositoryFailure, List[Project]] = {
    val clashing = markProjectsWithClashingIds(projects)
    Traverse[List].traverse(clashing) { case (isClashing, project) =>
      if (isClashing) {
        for {
          newId <- gen.randomUUID()
          updatedProject = project.copy(id = newId)
          _ <- update(updatedProject)
        } yield updatedProject
      } else {
        Applicative[F].pure(project)
      }
    }
  }

  /** Take a list of projects and mark the projects that have duplicate ids.
    *
    * @param projects the list of projects
    * @return the list of pairs. Fist element of the pair indicates if the
    * project has clashing id.
    */
  private def markProjectsWithClashingIds(
    projects: List[Project]
  ): List[(Boolean, Project)] = {
    projects.groupBy(_.id).foldRight(List.empty[(Boolean, Project)]) {
      case ((_, groupedProjects), acc) =>
        // groupBy always returns non-empty list
        (groupedProjects.sortBy(_.directoryCreationTime): @unchecked) match {
          case project :: clashingProjects =>
            (false, project) :: clashingProjects.map((true, _)) ::: acc
        }
    }
  }
}

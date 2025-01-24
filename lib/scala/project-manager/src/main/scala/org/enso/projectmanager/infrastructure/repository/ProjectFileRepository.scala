package org.enso.projectmanager.infrastructure.repository

import java.io.File
import java.nio.file.Path
import java.nio.file.attribute.FileTime
import java.util.UUID
import org.enso.pkg.{Package, PackageManager}
import org.enso.pkg.validation.NameValidation
import org.enso.projectmanager.boot.configuration.MetadataStorageConfig
import org.enso.projectmanager.control.core.{
  Applicative,
  CovariantFlatMap,
  Traverse
}
import org.enso.projectmanager.control.core.syntax._
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.desktop.TrashCan
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
  * @param metadataStorageConfig a metadata storage config
  * @param clock a clock
  * @param fileSystem a file system abstraction
  * @param gen a random generator
  * @param trash a trash can
  */
class ProjectFileRepository[
  F[+_, +_]: Sync: ErrorChannel: CovariantFlatMap: Applicative
](
  projectsPath: File,
  metadataStorageConfig: MetadataStorageConfig,
  clock: Clock[F],
  fileSystem: FileSystem[F],
  gen: Generator[F],
  trash: TrashCan[F]
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
      .list(projectsPath)
      .map(_.filter(_.isDirectory))
      .recover { case FileNotFound | NotDirectory =>
        Nil
      }
      .mapError(th =>
        StorageFailure(s"Cannot find projects at $projectsPath: ${th.toString}")
      )
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
    moduleName: String
  ): F[ProjectRepositoryFailure, Path] =
    findTargetPath(moduleName).map(_.toPath)

  /** @inheritdoc */
  override def tryLoadProject(
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
        module                = pkg.normalizedName,
        namespace             = pkg.namespace,
        kind                  = meta.kind,
        created               = meta.created,
        edition               = pkg.getConfig().edition,
        lastOpened            = meta.lastOpened,
        path                  = directory,
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
        renamePackage(project.path, name)
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
      projectPackage <- getPackage(project.path)
    } yield projectPackage.getConfig().moduleName
  }

  /** @inheritdoc */
  def getPackageNamespace(
    projectId: UUID
  ): F[ProjectRepositoryFailure, String] = {
    for {
      project        <- getProject(projectId)
      projectPackage <- getPackage(project.path)
    } yield projectPackage.getConfig().namespace
  }

  private def loadPackage(
    projectPath: File
  ): F[ProjectRepositoryFailure, Option[Package[File]]] =
    Sync[F]
      .blockingOp { PackageManager.Default.fromDirectory(projectPath) }
      .mapError(th => StorageFailure(s"Cannot load package $projectPath: $th"))

  private def getDirectoryCreationTime(
    pkg: Package[File]
  ): F[ProjectRepositoryFailure, FileTime] =
    Sync[F]
      .blockingOp(pkg.fileSystem.getCreationTime(pkg.root))
      .mapError(th =>
        StorageFailure(
          s"Cannot find creation time for package ${pkg.root}: $th"
        )
      )

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
          .mapError(th =>
            StorageFailure(
              s"Cannot rename package $newName at $projectPath: $th"
            )
          )
      }

  /** @inheritdoc */
  def update(project: Project): F[ProjectRepositoryFailure, Unit] =
    metadataStorage(project.path)
      .persist(ProjectMetadata(project))
      .mapError(th =>
        StorageFailure(s"Cannot update project at ${project.path}: $th")
      )

  /** @inheritdoc */
  override def delete(projectId: UUID): F[ProjectRepositoryFailure, Unit] = {
    findById(projectId)
      .flatMap {
        case Some(project) =>
          fileSystem
            .remove(project.path)
            .mapError(th =>
              StorageFailure(s"Cannot delete project at ${project.path}: $th")
            )
        case None =>
          ErrorChannel[F].fail(ProjectNotFoundInIndex)
      }
  }

  /** @inheritdoc */
  override def moveToTrash(
    projectId: UUID
  ): F[ProjectRepositoryFailure, Boolean] = {
    findById(projectId)
      .flatMap {
        case Some(project) =>
          trash.moveToTrash(project.path)
        case None =>
          ErrorChannel[F].fail(ProjectNotFoundInIndex)
      }
  }

  /** @inheritdoc */
  override def moveProject(
    projectId: UUID,
    newName: String
  ): F[ProjectRepositoryFailure, File] = {
    def move(project: Project) =
      for {
        targetPath <- findTargetPath(NameValidation.normalizeName(newName))
        _          <- moveProjectDir(project.path, targetPath)
      } yield targetPath

    for {
      project     <- getProject(projectId)
      projectPath <- move(project)
    } yield projectPath
  }

  /** @inheritdoc */
  override def copyProject(
    project: Project,
    newName: String,
    newMetadata: ProjectMetadata
  ): F[ProjectRepositoryFailure, Project] = {
    def copy(project: Project) =
      for {
        targetPath <- findTargetPath(NameValidation.normalizeName(newName))
        _          <- copyProjectDir(project.path, targetPath)
      } yield targetPath

    for {
      newProjectPath <- copy(project)
      _ <- metadataStorage(newProjectPath)
        .persist(newMetadata)
        .mapError(th =>
          StorageFailure(
            s"Cannot persist new project name at $newProjectPath: $th"
          )
        )
      _          <- renamePackage(newProjectPath, newName)
      newProject <- getProject(newMetadata.id)
    } yield newProject
  }

  private def moveProjectDir(projectPath: File, targetPath: File) = {
    fileSystem
      .move(projectPath, targetPath)
      .mapError[ProjectRepositoryFailure](failure =>
        StorageFailure(
          s"Cannot move project path from $projectPath to $targetPath: $failure"
        )
      )
  }

  private def copyProjectDir(projectPath: File, targetPath: File) = {
    fileSystem
      .copy(projectPath, targetPath)
      .mapError[ProjectRepositoryFailure](failure =>
        StorageFailure(
          s"Cannot copy project directory $projectPath to $targetPath: $failure"
        )
      )
  }

  private def findTargetPath(
    moduleName: String
  ): F[ProjectRepositoryFailure, File] =
    CovariantFlatMap[F]
      .tailRecM[ProjectRepositoryFailure, Int, File](0) { number =>
        val path =
          new File(
            projectsPath,
            moduleName + genSuffix(number)
          )
        fileSystem
          .exists(path)
          .mapError[ProjectRepositoryFailure](failure =>
            StorageFailure(s"Cannot find path $path: $failure")
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
      metadataStorageConfig,
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
          newId          <- gen.randomUUID()
          updatedProject <- Applicative[F].pure(project.copy(id = newId))
          _              <- update(updatedProject)
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

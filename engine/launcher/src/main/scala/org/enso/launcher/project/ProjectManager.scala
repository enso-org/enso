package org.enso.launcher.project

import org.enso.pkg.PackageManager
import org.enso.runtimeversionmanager.runner.Project

import java.nio.file.Path

import scala.util.{Failure, Try}

/** A helper class for project management.
  *
  * It allows to create new project, open existing ones or traverse the
  * directory tree to find a project based on a path inside it.
  */
class ProjectManager {

  private val packageManager = PackageManager.Default

  /** Tries to load the project at the provided `path`.
    */
  def loadProject(path: Path): Try[Project] =
    packageManager
      .loadPackage(path.toFile)
      .map(new Project(_))
      .recoverWith(error => Failure(ProjectLoadingError(path, error)))

  /** Traverses the directory tree looking for a project in one of the ancestors
    * of the provided `path`.
    *
    * If a package file is missing in a directory, its ancestors are searched
    * recursively. However if a package file exists in some directory, but there
    * are errors preventing from loading it, that error is reported.
    */
  def findProject(path: Path): Try[Option[Project]] =
    tryFindingProject(path.toAbsolutePath.normalize).map(Some(_)).recover {
      case PackageManager.PackageNotFound() => None
    }

  private def tryFindingProject(root: Path): Try[Project] =
    packageManager
      .loadPackage(root.toFile)
      .map(new Project(_))
      .recoverWith {
        case PackageManager.PackageNotFound() if root.getParent != null =>
          tryFindingProject(root.getParent)
        case otherError => Failure(otherError)
      }
}

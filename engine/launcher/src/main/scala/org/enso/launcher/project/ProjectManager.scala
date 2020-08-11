package org.enso.launcher.project

import java.nio.file.Path

import org.enso.launcher.{GlobalConfigurationManager, Logger}
import org.enso.pkg.PackageManager

import scala.util.Try

/**
  * A helper class for project management.
  *
  * It allows to create new project, open existing ones or traverse the
  * directory tree to find a project based on a path inside it.
  */
class ProjectManager(globalConfigurationManager: GlobalConfigurationManager) {

  private val packageManager = PackageManager.Default

  /**
    * Creates a new project at the specified path with the given name.
    *
    * If the version is not provided, the default Enso engine version is used.
    *
    * @param name specifies the name of the project
    * @param path specifies where the project should be created
    * @param ensoVersion if provided, specifies an exact Enso version that the
    *                    project should be associated with
    */
  def newProject(
    name: String,
    path: Path,
    ensoVersion: Option[String] = None
  ): Unit = {
    packageManager.create(
      root = path.toFile,
      name = name,
      ensoVersion = ensoVersion.getOrElse {
        globalConfigurationManager.defaultVersion.toString()
      }
    )
    Logger.info(s"Project created in `$path`.")
  }

  /**
    * Tries to load the project at the provided path.
    */
  def loadProject(path: Path): Try[Project] =
    packageManager
      .fromDirectory(path.toFile)
      .map(new Project(_, globalConfigurationManager))
      .toRight(ProjectLoadingError(path))
      .toTry

  /**
    * Traverses the directory tree looking for a project in one of the ancestors
    * of the provided path.
    */
  def findProject(path: Path): Option[Project] =
    tryFindingProject(path.toAbsolutePath.normalize)

  private def tryFindingProject(root: Path): Option[Project] =
    packageManager.fromDirectory(root.toFile) match {
      case Some(found) =>
        Some(new Project(found, globalConfigurationManager))
      case None =>
        Option(root.getParent).flatMap(tryFindingProject)
    }
}

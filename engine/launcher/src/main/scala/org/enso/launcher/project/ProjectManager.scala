package org.enso.launcher.project

import java.nio.file.Path

import org.enso.launcher.{GlobalConfigurationManager, Logger}
import org.enso.pkg.PackageManager

import scala.util.Try

class ProjectManager(globalConfigurationManager: GlobalConfigurationManager) {

  private val packageManager = PackageManager.Default

  def newProject(name: String, path: Path): Unit = {
    val ensoVersion = globalConfigurationManager.defaultVersion
    packageManager.create(
      root        = path.toFile,
      name        = name,
      ensoVersion = ensoVersion.toString()
    )
    Logger.info(s"Project created in `$path`.")
  }

  def loadProject(path: Path): Try[Project] =
    packageManager
      .fromDirectory(path.toFile)
      .map(new Project(_, globalConfigurationManager))
      .toRight(ProjectLoadingError(path))
      .toTry

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

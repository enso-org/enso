package org.enso.launcher.project

import java.nio.file.Path

import org.enso.launcher.{GlobalConfigurationManager, Logger}
import org.enso.pkg.PackageManager

class ProjectManager(globalConfigurationManager: GlobalConfigurationManager) {

  private val packageManager = PackageManager.Default

  def newProject(name: String, path: Path): Unit = {
    val ensoVersion = globalConfigurationManager.defaultVersion
    packageManager.create(
      root        = path.toFile,
      name        = name,
      ensoVersion = ensoVersion.toString()
    )
    Logger.info(s"Project created in $path")
  }

  def findCurrentProject(): Option[Project] = {
    def tryFindingProject(root: Path): Option[Project] =
      packageManager.fromDirectory(root.toFile) match {
        case Some(found) =>
          Some(new Project(found))
        case None =>
          Option(root.getParent).flatMap(tryFindingProject)
      }

    tryFindingProject(Path.of(".").toAbsolutePath.normalize)
  }
}

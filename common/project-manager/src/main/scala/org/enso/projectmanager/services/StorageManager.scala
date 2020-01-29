package org.enso.projectmanager.services

import java.io.File

import org.enso.pkg.Package
import org.enso.projectmanager.model._

case class StorageManager(
  localProjectsPath: File,
  tmpProjectsPath: File,
  tutorialsPath: File) {

  localProjectsPath.mkdirs()
  tmpProjectsPath.mkdirs()
  tutorialsPath.mkdirs()

  def moveToLocal(project: Project, newName: Option[String]): Project = {
    val pkg     = project.pkg
    val renamed = newName.map(pkg.rename).getOrElse(pkg)
    val root    = createRootForName(localProjectsPath, renamed.name)
    val moved   = renamed.move(root)
    Project(Local, moved)
  }

  def createRootForName(
    rootDir: File,
    name: String,
    idx: Option[Int] = None
  ): File = {
    val idxSuffix = idx.map(idx => s".$idx").getOrElse("")
    val nameToTry = name + idxSuffix
    val rootToTry = new File(rootDir, nameToTry)

    if (rootToTry.mkdirs()) rootToTry
    else {
      val nextIdx = idx.map(_ + 1).getOrElse(0)
      createRootForName(rootDir, name, Some(nextIdx))
    }

  }

  def readLocalProjects: ProjectsRepository =
    listProjectsInDirectory(Local, localProjectsPath)

  def readTutorials: ProjectsRepository =
    listProjectsInDirectory(Tutorial, tutorialsPath)

  def listProjectsInDirectory(
    kind: ProjectType,
    dir: File
  ): ProjectsRepository = {
    val candidates = dir.listFiles(_.isDirectory).toList
    val projects   = candidates.map(Package.getOrCreate).map(Project(kind, _))
    ProjectsRepository(projects)
  }

  def createTemporary(name: String): Project = {
    val root = createRootForName(tmpProjectsPath, name)
    val pkg  = Package.create(root, name)
    Project(Temporary, pkg)
  }
}

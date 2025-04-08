package org.enso.projectmanager.boot.configuration

import org.enso.os.environment.DesktopEnvironment

import java.io.{File, IOException}

/** A configuration object for properties of project storage.
  *
  * @param projectsRoot overrides user projects root directory
  * @param projectsDirectory a user projects directory
  * @param metadata a metadata storage config
  */
case class StorageConfig(
  projectsRoot: Option[File],
  projectsDirectory: String,
  metadata: MetadataStorageConfig
) {

  /** @return a path to the user projects directory. */
  @throws[IOException]
  def userProjectsPath: File = {
    val projectsRootDirectory =
      projectsRoot.getOrElse(
        DesktopEnvironment.getDirectories.getDocuments.toFile
      )
    new File(projectsRootDirectory, projectsDirectory)
  }
}

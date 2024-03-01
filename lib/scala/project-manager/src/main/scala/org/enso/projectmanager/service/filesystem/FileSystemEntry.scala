package org.enso.projectmanager.service.filesystem

import org.enso.projectmanager.data.ProjectMetadata

import java.io.File

/** Base trait for the file system entries. */
sealed trait FileSystemEntry {

  /** A path to the file system entry. */
  def path: File
}

object FileSystemEntry {

  /** A file.
    *
    * @param path the path to the file
    */
  case class FileEntry(path: File) extends FileSystemEntry

  /** A directory.
    *
    * @param path the path to the directory
    */
  case class DirectoryEntry(path: File) extends FileSystemEntry

  /** A directory containing a project.
    *
    * @param path the path to the directory
    * @param metadata the project metadata
    */
  case class ProjectEntry(path: File, metadata: ProjectMetadata)
      extends FileSystemEntry
}

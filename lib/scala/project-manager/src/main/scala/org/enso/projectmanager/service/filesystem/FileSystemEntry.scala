package org.enso.projectmanager.service.filesystem

import org.enso.projectmanager.data.ProjectMetadata

import java.io.File

sealed trait FileSystemEntry {

  def path: File
}

object FileSystemEntry {

  case class FileEntry(path: File) extends FileSystemEntry

  case class DirectoryEntry(path: File) extends FileSystemEntry

  case class ProjectEntry(path: File, metadata: ProjectMetadata)
      extends FileSystemEntry
}

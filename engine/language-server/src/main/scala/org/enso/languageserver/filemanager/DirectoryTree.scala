package org.enso.languageserver.filemanager

import java.io.File

/**
  * A representation of tree structures of files and directories.
  *
  * @param path to the directory
  * @param name of the directory
  * @param files contents of the directory
  * @param directories contents of the directory
  */
case class DirectoryTree(
  path: Path,
  name: String,
  files: Vector[FileSystemObject],
  directories: Vector[DirectoryTree]
)

object DirectoryTree {

  /**
    * Create [[DirectoryTree]] from [[FileSystemApi.DirectoryEntry]]
    * converting absolute pathes to ones relative to project root.
    *
    * @param root path to the project root
    * @param base path to the required directory
    * @param directory a [[FileSystemApi]] representation of a directory
    * @return a directory tree with paths relative to project root
    */
  def fromDirectoryEntry(
    root: File,
    base: Path,
    directory: FileSystemApi.DirectoryEntry
  ): DirectoryTree =
    DirectoryTree(
      path  = Path.getRelativeParent(root, base, directory.path),
      name  = directory.path.getFileName.toString,
      files = directory.children.flatMap(toFile(root, base, _)).toVector,
      directories =
        directory.children.flatMap(toDirectory(root, base, _)).toVector
    )

  private def toDirectory(
    root: File,
    base: Path,
    entry: FileSystemApi.Entry
  ): Option[DirectoryTree] =
    entry match {
      case dir: FileSystemApi.DirectoryEntry =>
        Some(fromDirectoryEntry(root, base, dir))
      case _ =>
        None
    }

  private def toFile(
    root: File,
    base: Path,
    entry: FileSystemApi.Entry
  ): Option[FileSystemObject] =
    entry match {
      case _: FileSystemApi.DirectoryEntry =>
        None
      case entry =>
        Some(FileSystemObject.fromEntry(root, base, entry))
    }
}

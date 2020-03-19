package org.enso.languageserver.filemanager

import java.io.File
import java.nio.file.Path

import scala.collection.mutable.ArrayBuffer

/**
  * File manipulation API.
  *
  * @tparam F represents target monad
  */
trait FileSystemApi[F[_, _]] {

  import FileSystemApi._

  /**
    * Writes textual content to a file.
    *
    * @param file path to the file
    * @param content a textual content of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def write(
    file: File,
    content: String
  ): F[FileSystemFailure, Unit]

  /**
    * Reads the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  def read(file: File): F[FileSystemFailure, String]

  /**
    * Deletes the specified file or directory recursively.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def delete(file: File): F[FileSystemFailure, Unit]

  /**
    * Creates an empty file with parent directory.
    *
    * @param file path to the file
    * @return
    */
  def createFile(file: File): F[FileSystemFailure, Unit]

  /**
    * Creates a directory, including any necessary but nonexistent parent
    * directories.
    *
    * @param file path to the file
    * @return
    */
  def createDirectory(file: File): F[FileSystemFailure, Unit]

  /**
    * Copy a file or directory recursively
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def copy(
    from: File,
    to: File
  ): F[FileSystemFailure, Unit]

  /**
    * Move a file or directory recursively
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def move(
    from: File,
    to: File
  ): F[FileSystemFailure, Unit]

  /**
    * Checks if the specified file exists.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or file existence flag
    */
  def exists(file: File): F[FileSystemFailure, Boolean]

  /**
    * List contents of a given path.
    *
    * @param path to the file system object
    * @return either [[FileSystemFailure]] or list of entries
    */
  def list(path: File): F[FileSystemFailure, Vector[Entry]]

  /**
    * Returns contents of a given path.
    *
    * @param path to the file system object
    * @param depth maximum depth of a directory tree
    * @return either [[FileSystemFailure]] or directory structure
    */
  def tree(
    path: File,
    depth: Option[Int]
  ): F[FileSystemFailure, DirectoryEntry]
}

object FileSystemApi {

  /**
    * An object representing abstract file system entry.
    */
  sealed trait Entry {
    def path: Path
  }

  /**
    * An entry representing a directory.
    *
    * @param path to the directory
    * @children a paths to the children entries
    */
  case class DirectoryEntry(path: Path, children: ArrayBuffer[Entry])
      extends Entry

  object DirectoryEntry {

    def empty(path: Path): DirectoryEntry =
      DirectoryEntry(path, ArrayBuffer())
  }

  /**
    * An entry representing a directory with contents truncated.
    *
    * @param path to the directory
    */
  case class DirectoryEntryTruncated(path: Path) extends Entry

  /**
    * An entry representing a symbolic link.
    *
    * @param path to the symlink
    * @param target of the symlink.
    */
  case class SymbolicLinkEntry(path: Path, target: Path) extends Entry

  /**
    * An entry representing a file.
    *
    * @param path to the file
    */
  case class FileEntry(path: Path) extends Entry

  /**
    * Unrecognized file system entry. Example is a broken symlink.
    */
  case class OtherEntry(path: Path) extends Entry

}

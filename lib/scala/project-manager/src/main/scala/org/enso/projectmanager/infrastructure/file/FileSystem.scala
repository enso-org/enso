package org.enso.projectmanager.infrastructure.file

import java.io.{File, InputStream, OutputStream}

/** Represents abstraction for filesystem operations.
  *
  * @tparam F target bifunctor
  */
trait FileSystem[F[+_, +_]] {

  /** Read the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  def readTextFile(file: File): F[FileSystemFailure, String]

  /** Read the contents of a textual file to the provided output.
    *
    * @param file path to the file
    * @param output the output stream consuming the file contents
    * @return either [[FileSystemFailure]] or the number of bytes read
    */
  def readFile(file: File, output: OutputStream): F[FileSystemFailure, Int]

  /** Writes binary content to a file.
    *
    * @param file path to the file
    * @param contents a contents of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def writeFile(file: File, contents: InputStream): F[FileSystemFailure, Unit]

  /** Writes textual content to a file.
    *
    * @param file path to the file
    * @param contents a textual contents of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def overwriteFile(
    file: File,
    contents: String
  ): F[FileSystemFailure, Unit]

  /** Creates the specified directory with all required parent directories.
    *
    * @param path a path to the directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def createDir(path: File): F[FileSystemFailure, Unit]

  /** Deletes the specified file or directory recursively.
    *
    * @param path a path to the file or directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def remove(path: File): F[FileSystemFailure, Unit]

  /** Move a file or directory recursively.
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def move(from: File, to: File): F[FileSystemFailure, Unit]

  /** Copy a file or directory recursively.
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def copy(from: File, to: File): F[FileSystemFailure, Unit]

  /** Tests if a file exists.
    *
    * @param file the file to check
    * @return true if exists
    */
  def exists(file: File): F[FileSystemFailure, Boolean]

  /** List files in the directory.
    *
    * @param directory a path to the directory
    * @return the directory contents
    */
  def list(directory: File): F[FileSystemFailure, List[File]]
}

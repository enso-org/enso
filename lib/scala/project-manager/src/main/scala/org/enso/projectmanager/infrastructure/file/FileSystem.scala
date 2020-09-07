package org.enso.projectmanager.infrastructure.file

import java.io.File

/**
  * Represents abstraction for filesystem operations.
  *
  * @tparam F target bifunctor
  */
trait FileSystem[F[+_, +_]] {

  /**
    * Reads the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  def readFile(file: File): F[FileSystemFailure, String]

  /**
    * Writes textual content to a file.
    *
    * @param file path to the file
    * @param contents a textual contents of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def overwriteFile(
    file: File,
    contents: String
  ): F[FileSystemFailure, Unit]

  /**
    * Deletes the specified directory recursively.
    *
    * @param path a path to the directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def removeDir(path: File): F[FileSystemFailure, Unit]

  /**
    * Move a file or directory recursively
    *
    * @param from a path to the source
    * @param to a path to the destination
    * @return either [[FileSystemFailure]] or Unit
    */
  def move(from: File, to: File): F[FileSystemFailure, Unit]

  /**
    * Tests if a file exists.
    *
    * @param file the file to check
    * @return true if exists
    */
  def exists(file: File): F[FileSystemFailure, Boolean]

  /**
    * List files in the directory.
    *
    * @param directory a path to the directory
    * @return the directory contents
    */
  def list(directory: File): F[FileSystemFailure, List[File]]
}

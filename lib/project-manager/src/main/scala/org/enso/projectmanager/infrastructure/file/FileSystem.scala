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

}

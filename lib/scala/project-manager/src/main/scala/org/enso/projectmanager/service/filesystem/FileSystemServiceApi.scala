package org.enso.projectmanager.service.filesystem

import java.io.File

trait FileSystemServiceApi[F[+_, +_]] {

  /** List file system entries in the provided directory
    *
    * @param path the directory to list
    * @return the list of file system entries in the provided directory
    */
  def list(path: File): F[FileSystemServiceFailure, Seq[FileSystemEntry]]

  /** Create a directory with required parent directories.
    *
    * @param path the directory to create
    */
  def createDirectory(path: File): F[FileSystemServiceFailure, Unit]

  /** Deletes a directory with its contents.
    *
    * @param path the directory to delete
    */
  def deleteDirectory(path: File): F[FileSystemServiceFailure, Unit]

  /** Moves a file or directory recursively.
    *
    * @param from the target path
    * @param to the destination path
    */
  def move(from: File, to: File): F[FileSystemServiceFailure, Unit]
}

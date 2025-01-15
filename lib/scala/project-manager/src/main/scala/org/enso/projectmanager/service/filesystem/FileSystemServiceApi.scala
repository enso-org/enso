package org.enso.projectmanager.service.filesystem

import java.io.{File, InputStream, OutputStream}

trait FileSystemServiceApi[F[+_, +_]] {

  /** Checks if the file or directory exists.
    *
    * @param path the file or directory to check
    * @return true if the file or directory exists, false otherwise
    */
  def exists(path: File): F[FileSystemServiceFailure, Boolean]

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

  /** Deletes a file or a directory with its contents.
    *
    * @param path the file or directory to delete
    */
  def delete(path: File): F[FileSystemServiceFailure, Unit]

  /** Moves a file or directory recursively.
    *
    * @param from the target path
    * @param to the destination path
    */
  def move(from: File, to: File): F[FileSystemServiceFailure, Unit]

  /** Copy a file or directory recursively.
    *
    * @param from the target path
    * @param to the destination path
    */
  def copy(from: File, to: File): F[FileSystemServiceFailure, Unit]

  /** Read a file to the provided output.
    *
    * @param path the file path to write
    * @param out the output consuming the file contents
    * @return the number of bytes read
    */
  def read(path: File, out: OutputStream): F[FileSystemServiceFailure, Int]

  /** Writes a file
    *
    * @param path the file path to write
    * @param in the file contents
    */
  def write(path: File, in: InputStream): F[FileSystemServiceFailure, Unit]
}

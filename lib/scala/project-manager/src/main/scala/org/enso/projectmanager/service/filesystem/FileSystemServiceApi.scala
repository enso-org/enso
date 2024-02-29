package org.enso.projectmanager.service.filesystem

import java.io.File

trait FileSystemServiceApi[F[+_, +_]] {

  /** List file system entries in the provided directory
    *
    * @param path the directory to list
    * @return the list of file system entries in the provided directory
    */
  def list(path: File): F[FileSystemServiceFailure, Seq[FileSystemEntry]]
}

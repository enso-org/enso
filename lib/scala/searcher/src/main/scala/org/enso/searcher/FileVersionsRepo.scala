package org.enso.searcher

import java.io.File

/** The object for accessing the database containing the file versions. */
trait FileVersionsRepo[F[_]] {

  /** Get the file version.
    *
    * @param file the file path
    * @return the version digest
    */
  def getVersion(file: File): F[Option[Array[Byte]]]

  /** Set the file version.
    *
    * @param file the file path
    * @param digest the version digest
    * @return previously recorded file version
    */
  def setVersion(file: File, digest: Array[Byte]): F[Option[Array[Byte]]]

  /** Update the version if it differs from the recorded version.
    *
    * @param file the file path
    * @param digest the version digest
    * @return `true` if the version has been updated
    */
  def updateVersion(file: File, digest: Array[Byte]): F[Boolean]

  /** Remove the version record.
    *
    * @param file the file path
    */
  def remove(file: File): F[Unit]

  /** Clean the repo. */
  def clean: F[Unit]
}

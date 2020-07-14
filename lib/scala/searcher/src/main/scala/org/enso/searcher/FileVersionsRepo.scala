package org.enso.searcher

import java.io.File

trait FileVersionsRepo[F[_]] {

  def getVersion(file: File): F[Option[Array[Byte]]]

  def setVersion(file: File, digest: Array[Byte]): F[Option[Array[Byte]]]

  def remove(file: File): F[Unit]
}

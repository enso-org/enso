package org.enso.launcher.archive

import java.nio.file.Path

sealed trait ArchiveFormat
object ArchiveFormat {
  case object ZIP   extends ArchiveFormat
  case object TarGz extends ArchiveFormat

  def detect(path: Path): Option[ArchiveFormat] = {
    val fileName = path.getFileName.toString
    if (fileName.endsWith(".zip")) Some(ZIP)
    else if (fileName.endsWith(".tar.gz") || fileName.endsWith(".tgz"))
      Some(TarGz)
    else None
  }
}

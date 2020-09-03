package org.enso.launcher.archive

import java.nio.file.Path

trait ArchiveEntry {
  def relativePath:                 Path
  def extractTo(destination: Path): Unit
}

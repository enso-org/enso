package org.enso.launcher.archive

import java.nio.file.Path

trait ArchiveEntry {
  def isDirectory:                  Boolean
  def relativePath:                 Path
  def extractTo(destination: Path): Unit
}

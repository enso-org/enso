package org.enso.launcher.project

import java.nio.file.Path

case class ProjectLoadingError(path: Path)
    extends RuntimeException(s"Cannot load an Enso project at `$path`.") {
  override def toString: String = getMessage
}

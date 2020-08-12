package org.enso.launcher.project

import java.nio.file.Path

/**
  * Indicates that it was impossible to load the project at a specified path.
  */
case class ProjectLoadingError(path: Path, cause: Throwable)
    extends RuntimeException(
      s"Cannot load an Enso project at `$path` due to: $cause",
      cause
    ) {

  /**
    * @inheritdoc
    */
  override def toString: String = getMessage
}

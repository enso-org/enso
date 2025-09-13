package org.enso.interpreter.runtime.util

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

private[util] object RepositoryFinder {
  @tailrec
  private def findRepositoryRoot(path: Path): Option[Path] = {
    val gitDir = path.resolve(".git")
    if (gitDir.toFile.exists()) {
      Some(path)
    } else {
      val parent = path.getParent
      if (parent != null) {
        findRepositoryRoot(parent)
      } else {
        None
      }
    }
  }

  private lazy val root: Option[Path] = {
    val currentDir = Path.of(".").toAbsolutePath.normalize()
    findRepositoryRoot(currentDir)
  }

  /** Handles relativizing the path to the repository root and replacing the
    * built-distribution path with the libraries base source code path.
    *
    * This ensures that the actual source files are referenced instead of the build artifacts.
    */
  def rewritePath(path: Path): Path = {
    val relative = RepositoryFinder.root
      .map(_.relativize(path))
      .getOrElse(path)
    if (relative.startsWith("built-distribution")) {
      val rewrittenPath = Path.of(
        relative.toString
          .replace("\\", "/")
          .replaceFirst(
            """built-distribution/enso-engine-[^/]+/enso-[^/]+/lib/([^/]+)/([^/]+)/[^/]+/""",
            "distribution/lib/$1/$2/0.0.0-dev/"
          )
      )

      // Only apply the rewrite if the new path points to an existing file.
      if (Files.exists(rewrittenPath)) {
        rewrittenPath
      } else {
        relative
      }
    } else relative
  }
}

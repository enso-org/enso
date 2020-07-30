package org.enso.launcher.archive

import java.nio.file.Path

private[archive] class RootRenamer(newRoot: Path) extends (Path => Path) {
  var lastRoot: Option[Path] = None
  override def apply(path: Path): Path = {
    if (path.getNameCount < 1) {
      throw new RuntimeException(
        s"Unexpected archive structure - the file $path cannot " +
        s"have its root renamed."
      )
    }

    val oldRoot = path.getName(0)
    lastRoot match {
      case Some(lastRoot) =>
        if (lastRoot != oldRoot) {
          throw new RuntimeException(
            s"Unexpected archive structure - the archive contains " +
            s"more than one file in the root - $lastRoot, $oldRoot"
          )
        }
      case None =>
        lastRoot = Some(oldRoot)
    }

    if (path.getNameCount == 1)
      newRoot
    else {
      val remainingParts = path.subpath(1, path.getNameCount)
      newRoot.resolve(remainingParts)
    }
  }
}

package org.enso.launcher.archive

import java.io.FileInputStream
import java.nio.file.{Files, Path}

import org.enso.launcher.internal.ProgressInputStream

object FileProgressInputStream {
  def apply(path: Path): ProgressInputStream = {
    val size = Files.size(path)
    new ProgressInputStream(
      new FileInputStream(path.toFile),
      Some(size),
      _ => ()
    )
  }
}

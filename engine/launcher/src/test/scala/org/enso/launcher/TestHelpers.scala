package org.enso.launcher

import java.nio.file.Path

import scala.io.Source
import scala.util.Using

object TestHelpers {
  def readFileContent(path: Path): String = {
    Using(Source.fromFile(path.toFile)) { source =>
      source.getLines().mkString("\n")
    }.get
  }
}

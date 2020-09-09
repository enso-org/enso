package org.enso.launcher

import java.nio.file.Path

import scala.io.Source
import scala.util.Using

/**
  * Gathers helper functions for the test suite.
  */
object TestHelpers {

  /**
    * Reads file contents into a [[String]].
    */
  def readFileContent(path: Path): String = {
    Using(Source.fromFile(path.toFile)) { source =>
      source.getLines().mkString("\n")
    }.get
  }
}

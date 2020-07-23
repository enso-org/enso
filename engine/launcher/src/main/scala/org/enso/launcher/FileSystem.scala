package org.enso.launcher

import java.io.PrintWriter
import java.nio.file.{Files, Path}

import scala.collection.Factory
import scala.jdk.StreamConverters._

object FileSystem {
  def listDirectory(dir: Path): Seq[Path] = {
    Files.list(dir).toScala(Factory.arrayFactory).toSeq
  }

  def writeTextFile(path: Path, content: String): Unit = {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }
}

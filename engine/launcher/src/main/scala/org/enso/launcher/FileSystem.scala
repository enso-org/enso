package org.enso.launcher

import java.io.PrintWriter

import org.apache.commons.io.FileUtils
import java.nio.file.{Files, Path}

import org.enso.launcher.internal.OS

import scala.collection.Factory
import scala.jdk.StreamConverters._
import sys.process._

object FileSystem {
  def listDirectory(dir: Path): Seq[Path] =
    if (!Files.exists(dir)) Seq()
    else Files.list(dir).toScala(Factory.arrayFactory).toSeq

  def writeTextFile(path: Path, content: String): Unit = {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }

  def copyDirectory(source: Path, destination: Path): Unit =
    FileUtils.copyDirectory(source.toFile, destination.toFile)

  def removeDirectory(dir: Path): Unit =
    FileUtils.deleteDirectory(dir.toFile)

  def copyFile(source: Path, destination: Path): Unit =
    FileUtils.copyFile(source.toFile, destination.toFile)

  def ensureIsExecutable(file: Path): Unit = {
    if (!Files.isExecutable(file)) {
      def tryChmod(): Boolean = {
        Seq("chmod", "+x", file.toAbsolutePath.toString).! == 0
      }

      if (OS.isWindows || !tryChmod()) {
        throw new IllegalStateException(
          "Cannot ensure the launcher binary is executable."
        )
      }
    }
  }

  implicit class PathSyntax(val path: Path) extends AnyVal {
    def /(other: String): Path = path.resolve(other)
    def /(other: Path): Path   = path.resolve(other)
  }
}

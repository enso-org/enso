package org.enso.launcher

import java.io.PrintWriter

import org.apache.commons.io.FileUtils
import java.nio.file.{Files, Path}

import org.enso.launcher.internal.OS

import scala.collection.Factory
import scala.jdk.StreamConverters._
import sys.process._

/**
  * Gathers some helper methods that are used for interaction with the
  * filesystem.
  */
object FileSystem {

  /**
    * Returns a sequence of files in the given directory (without traversing it
    * recursively).
    *
    * If the directory does not exist, returns an empty sequence.
    */
  def listDirectory(dir: Path): Seq[Path] =
    if (!Files.exists(dir)) Seq()
    else Files.list(dir).toScala(Factory.arrayFactory).toSeq

  /**
    * Writes a String to a file at the given `path`, creating the file if
    * necessary.
    */
  def writeTextFile(path: Path, content: String): Unit = {
    val writer = new PrintWriter(path.toFile)
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }

  /**
    * Copies a directory recursively.
    */
  def copyDirectory(source: Path, destination: Path): Unit =
    FileUtils.copyDirectory(source.toFile, destination.toFile)

  /**
    * Removes a directory recursively.
    */
  def removeDirectory(dir: Path): Unit =
    FileUtils.deleteDirectory(dir.toFile)

  /**
    * Copies a file, overwriting the destination if it already existed.
    */
  def copyFile(source: Path, destination: Path): Unit =
    FileUtils.copyFile(source.toFile, destination.toFile)

  /**
    * Checks if the given `file` is executable and tries to fix it if it is not.
    */
  def ensureIsExecutable(file: Path): Unit = {
    if (!Files.isExecutable(file)) {
      def tryChmod(): Boolean = {
        Seq("chmod", "+x", file.toAbsolutePath.toString).! == 0
      }

      if (OS.isWindows || !tryChmod()) {
        Logger.error("Cannot ensure the launcher binary is executable.")
      }
    }
  }

  /**
    * Allows to write nested paths in a more readable and concise way.
    */
  implicit class PathSyntax(val path: Path) extends AnyVal {
    def /(other: String): Path = path.resolve(other)
    def /(other: Path): Path   = path.resolve(other)
  }
}

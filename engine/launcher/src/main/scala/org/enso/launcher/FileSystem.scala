package org.enso.launcher

import java.io.PrintWriter
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util

import org.apache.commons.io.FileUtils

import scala.collection.Factory
import scala.jdk.StreamConverters._
import scala.util.Using

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
    Using(new PrintWriter(path.toFile)) { writer => writer.write(content) }
  }

  /**
    * Copies a directory recursively.
    */
  def copyDirectory(source: Path, destination: Path): Unit =
    FileUtils.copyDirectory(source.toFile, destination.toFile)

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
      if (OS.isWindows) {
        Logger.error("Cannot ensure the launcher binary is executable.")
      } else {
        try {
          Files.setPosixFilePermissions(
            file.toAbsolutePath,
            PosixFilePermissions.fromString("rwxrwxr-x")
          )
        } catch {
          case e: Exception =>
            Logger.error(
              s"Cannot ensure the launcher binary is executable: $e",
              e
            )
        }
      }
    }
  }

  def decodePOSIXPermissions(mode: Int): java.util.Set[PosixFilePermission] = {
    val res =
      util.EnumSet.noneOf[PosixFilePermission](classOf[PosixFilePermission])

    val others = mode & 7
    val group  = (mode >> 3) & 7
    val owner  = mode >> 6

    if ((owner & 4) != 0) {
      res.add(PosixFilePermission.OWNER_READ)
    }
    if ((owner & 2) != 0) {
      res.add(PosixFilePermission.OWNER_WRITE)
    }
    if ((owner & 1) != 0) {
      res.add(PosixFilePermission.OWNER_EXECUTE)
    }

    if ((group & 4) != 0) {
      res.add(PosixFilePermission.GROUP_READ)
    }
    if ((group & 2) != 0) {
      res.add(PosixFilePermission.GROUP_WRITE)
    }
    if ((group & 1) != 0) {
      res.add(PosixFilePermission.GROUP_EXECUTE)
    }

    if ((others & 4) != 0) {
      res.add(PosixFilePermission.OTHERS_READ)
    }
    if ((others & 2) != 0) {
      res.add(PosixFilePermission.OTHERS_WRITE)
    }
    if ((others & 1) != 0) {
      res.add(PosixFilePermission.OTHERS_EXECUTE)
    }

    res
  }

  def withTemporaryDirectory[T](
    prefix: String = "enso"
  )(action: Path => T): T = {
    val dir = Files.createTempDirectory(prefix)
    try {
      action(dir)
    } finally {
      removeDirectory(dir)
    }
  }

  /**
    * Removes a directory recursively.
    */
  def removeDirectory(dir: Path): Unit =
    FileUtils.deleteDirectory(dir.toFile)

  def atomicMove(source: Path, destination: Path): Unit =
    Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE)

  /**
    * Allows to write nested paths in a more readable and concise way.
    */
  implicit class PathSyntax(val path: Path) extends AnyVal {
    def /(other: String): Path = path.resolve(other)
    def /(other: Path): Path   = path.resolve(other)
  }
}

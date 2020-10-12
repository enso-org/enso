package org.enso.launcher

import java.io.PrintWriter
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{
  DirectoryNotEmptyException,
  Files,
  Path,
  StandardCopyOption
}
import java.util

import org.apache.commons.io.FileUtils

import scala.collection.Factory
import scala.jdk.StreamConverters._
import scala.util.Using
import com.typesafe.scalalogging.Logger

/**
  * Gathers some helper methods that are used for interaction with the
  * filesystem.
  */
object FileSystem {

  private val logger = Logger[FileSystem.type]

  /**
    * Returns a sequence of files in the given directory (without traversing it
    * recursively).
    *
    * If the directory does not exist, returns an empty sequence.
    */
  def listDirectory(dir: Path): Seq[Path] =
    if (!Files.exists(dir)) Seq()
    else Using(Files.list(dir))(_.toScala(Factory.arrayFactory).toSeq).get

  /**
    * Writes a String to a file at the given `path`, creating the file if
    * necessary.
    */
  def writeTextFile(path: Path, content: String): Unit = {
    Using(new PrintWriter(path.toFile)) { writer => writer.write(content) }.get
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
        logger.error("Cannot ensure that the binary is executable.")
      } else {
        try {
          Files.setPosixFilePermissions(
            file.toAbsolutePath,
            PosixFilePermissions.fromString("rwxrwxr-x")
          )
        } catch {
          case e: Exception =>
            logger.error(
              s"Cannot ensure the binary is executable: $e",
              e
            )
        }
      }
    }
  }

  /**
    * Parses POSIX file permissions stored in a binary format into a set of Java
    * enumerations corresponding to these permissions.
    */
  def decodePOSIXPermissions(mode: Int): java.util.Set[PosixFilePermission] = {
    val res =
      util.EnumSet.noneOf[PosixFilePermission](classOf[PosixFilePermission])

    val others = mode & 7
    val group  = (mode >> 3) & 7
    val owner  = (mode >> 6) & 7

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

  /**
    * Runs the `action` with a parameter representing a temporary directory
    * created for it.
    *
    * The temporary directory is removed afterwards.
    *
    * @param prefix prefix to use for the temporary directory name
    * @param action action to execute with the directory
    * @tparam T type of action's result
    * @return result of running the `action`
    */
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

  /**
    * Removes a directory recursively, does not fail if it does not exist.
    */
  def removeDirectoryIfExists(dir: Path): Unit =
    if (Files.exists(dir))
      FileUtils.deleteDirectory(dir.toFile)

  /**
    * Removes a directory only if it is empty.
    *
    * Returned value indicates if the directory has been removed.
    */
  def removeDirectoryIfEmpty(dir: Path): Boolean =
    try {
      Files.delete(dir)
      true
    } catch {
      case _: DirectoryNotEmptyException =>
        false
    }

  /**
    * Removes a file, if it exists, does not fail if it does not exist.
    */
  def removeFileIfExists(path: Path): Unit =
    if (Files.exists(path))
      Files.delete(path)

  /**
    * Registers the directory to be removed when the program exits normally.
    *
    * The directory is only removed if it is empty.
    */
  def removeEmptyDirectoryOnExit(dir: Path): Unit =
    dir.toFile.deleteOnExit()

  /**
    * Checks if the directory contains any entries.
    */
  def isDirectoryEmpty(dir: Path): Boolean = {
    def hasEntries =
      Using(Files.newDirectoryStream(dir))(_.iterator().hasNext).get
    Files.isDirectory(dir) && !hasEntries
  }

  /**
    * Tries to move a directory from `source` to `destination` atomically.
    *
    * May not be actually atomic.
    */
  def atomicMove(source: Path, destination: Path): Unit = {
    Files.createDirectories(destination.getParent)
    Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE)
  }

  /**
    * Syntax allowing to write nested paths in a more readable and concise way.
    */
  implicit class PathSyntax(val path: Path) extends AnyVal {
    def /(other: String): Path = path.resolve(other)
    def /(other: Path): Path   = path.resolve(other)
  }
}

package org.enso.launcher

import java.io.{FileInputStream, PrintWriter}

import org.apache.commons.io.FileUtils
import java.nio.file.{Files, Path}

import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

import scala.collection.Factory
import scala.jdk.StreamConverters._
import scala.util.Using
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
    Using(new PrintWriter(path.toFile)) { writer => writer.write(content) }
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

  sealed trait ArchiveFormat
  object ArchiveFormat {
    case object ZIP   extends ArchiveFormat
    case object TarGz extends ArchiveFormat
  }
  def withOpenArchive[R](path: Path, format: ArchiveFormat)(
    action: ArchiveInputStream => R
  ): R = {
    Using(new FileInputStream(path.toFile)) { fileInputStream =>
      format match {
        case ArchiveFormat.ZIP =>
          Using(new ZipArchiveInputStream(fileInputStream))(action).get
        case ArchiveFormat.TarGz =>
          Using(new GzipCompressorInputStream(fileInputStream)) {
            compressorInputStream =>
              Using(new TarArchiveInputStream(compressorInputStream))(
                action
              ).get
          }.get
      }
    }.get
  }

  def repl(): Unit = {
//    extractArchive(
//      Path.of(
//        "/home/radeusgd/NBO/releasesci/enso-launcher-0.1.0-linux-amd64.tar.gz"
//      ),
//      ArchiveFormat.TarGz,
//      Path.of(
//        "./test_archive"
//      ),
//      None
//    )
    extractArchive(
      Path.of(
        "/home/radeusgd/NBO/releasesci/enso-engine-0.1.0.zip"
      ),
      ArchiveFormat.ZIP,
      Path.of(
        "./test_archive"
      ),
      None
    )
  }

  def extractArchive(
    archivePath: Path,
    format: ArchiveFormat,
    destinationDirectory: Path,
    renameRootFolder: Option[String]
  ): Unit = {
    val _ = (destinationDirectory, renameRootFolder)
    withOpenArchive(archivePath, format) { archive =>
      for (entry <- ArchiveIterator(archive)) {
        if (!archive.canReadEntryData(entry)) {
          Logger.warn(
            s"Cannot read ${entry.getName} from $archivePath. " +
            s"The archive may be corrupted."
          )
        } else {
          Logger.debug(s"Extracting ${entry.getName}")
        }
      }
    }
  }

//  private def resolvePathFromArchive(
//    fileName: String,
//    destinationDirectory: Path,
//    renameRootFolder: Option[String]
//  ): Path = {
//    ???
//  }

  private case class ArchiveIterator(archiveInputStream: ArchiveInputStream)
      extends Iterator[ArchiveEntry] {
    override def hasNext: Boolean = {
      findNext()
      nextEntry.isDefined
    }

    override def next(): ArchiveEntry = {
      findNext()
      nextEntry match {
        case Some(value) =>
          nextEntry = None
          value
        case None =>
          throw new NoSuchElementException("No more entries in the iterator.")
      }
    }

    private var nextEntry: Option[ArchiveEntry] = None
    private var finished: Boolean               = false
    private def findNext(): Unit = {
      if (nextEntry.isEmpty && !finished) {
        val nextCandidate = archiveInputStream.getNextEntry
        if (nextCandidate == null) {
          finished = true
        } else {
          nextEntry = Some(nextCandidate)
        }
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

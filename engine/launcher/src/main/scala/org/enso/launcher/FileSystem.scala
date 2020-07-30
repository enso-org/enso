package org.enso.launcher

import java.io.{BufferedInputStream, FileInputStream, InputStream, PrintWriter}

import org.apache.commons.io.FileUtils
import java.nio.file.{Files, Path}

import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.enso.cli.{TaskProgress, TaskProgressImplementation}

import scala.collection.Factory
import scala.jdk.StreamConverters._
import scala.util.{Try, Using}
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

  trait ReadProgress {
    def alreadyRead(): Long
    def total():       Long
  }

  private class FileProgressInputStream(path: Path) extends InputStream {
    private val size: Long      = Files.size(path)
    private val in              = new FileInputStream(path.toFile)
    private var bytesRead: Long = 0

    private val readProgress = new ReadProgress {
      override def alreadyRead(): Long = bytesRead
      override def total(): Long       = size
    }

    def progress: ReadProgress = readProgress

    override def available: Int =
      in.available()

    override def read: Int = {
      bytesRead += 1
      in.read()
    }

    override def read(b: Array[Byte]): Int = {
      val bytes = in.read(b)
      bytesRead += bytes
      bytes
    }

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      val bytes = in.read(b, off, len)
      bytesRead += bytes
      bytes
    }

    override def skip(n: Long): Long = {
      val skipped = in.skip(n)
      bytesRead += skipped
      skipped
    }

    override def close(): Unit =
      in.close()
  }

  def withOpenArchive[R](path: Path, format: ArchiveFormat)(
    action: (ArchiveInputStream, ReadProgress) => R
  ): Try[R] = {
    Using(new FileProgressInputStream(path)) { progressInputStream =>
      Using(new BufferedInputStream(progressInputStream)) { buffered =>
        format match {
          case ArchiveFormat.ZIP =>
            Using(new ZipArchiveInputStream(buffered))(
              action(_, progressInputStream.progress)
            ).get
          case ArchiveFormat.TarGz =>
            Using(new GzipCompressorInputStream(buffered)) {
              compressorInputStream =>
                Using(new TarArchiveInputStream(compressorInputStream))(
                  action(_, progressInputStream.progress)
                ).get
            }.get
        }
      }.get
    }
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
    // "/home/radeusgd/NBO/releasesci/enso-engine-0.1.0.zip"
    extractArchive(
      Path.of(
        "/home/radeusgd/NBO/JVM/graalvm-ce-java11-linux-amd64-20.1.0.tar.gz"
      ),
      ArchiveFormat.TarGz,
      Path.of(
        "./test_archive"
      ),
      None
    ).waitForResult(true)
  }

  def extractArchive(
    archivePath: Path,
    format: ArchiveFormat,
    destinationDirectory: Path,
    renameRootFolder: Option[String]
  ): TaskProgress[Unit] = {
    val _            = (destinationDirectory, renameRootFolder)
    val taskProgress = new TaskProgressImplementation[Unit]
    def runExtraction(): Unit = {
      val result = withOpenArchive(archivePath, format) { (archive, progress) =>
        for (entry <- ArchiveIterator(archive)) {
          if (!archive.canReadEntryData(entry)) {
            Logger.warn(
              s"Cannot read ${entry.getName} from $archivePath. " +
              s"The archive may be corrupted."
            )
          } else {
            // Logger.debug(s"Extracting ${entry.getName}")
          }

          taskProgress.reportProgress(
            progress.alreadyRead(),
            Some(progress.total())
          )
        }
      }
      taskProgress.setComplete(result)
    }
    val thread = new Thread(() => runExtraction())
    thread.start()
    taskProgress
  }

  private def parseArchiveName(name: String): Path =
    Path.of(name)

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

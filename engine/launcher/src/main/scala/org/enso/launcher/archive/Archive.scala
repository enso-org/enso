package org.enso.launcher.archive

import java.io.BufferedInputStream
import java.nio.file.{Files, Path}

import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream}
import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveInputStream
}
import org.apache.commons.compress.archivers.zip.{
  ZipArchiveEntry,
  ZipArchiveInputStream
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.commons.io.IOUtils
import org.enso.cli.{TaskProgress, TaskProgressImplementation}
import org.enso.launcher.{FileSystem, Logger, OS}

import scala.util.{Try, Using}

object Archive {
  def extractArchive(
    archivePath: Path,
    destinationDirectory: Path,
    renameRootFolder: Option[Path]
  ): TaskProgress[Unit] = {
    val format = ArchiveFormat.detect(archivePath)
    format
      .map(
        extractArchive(archivePath, _, destinationDirectory, renameRootFolder)
      )
      .getOrElse {
        TaskProgress.immediateFailure(
          new IllegalArgumentException(
            s"Could not detect archive format for " +
            s"${archivePath.getFileName}"
          )
        )
      }
  }

  def extractArchive(
    archivePath: Path,
    format: ArchiveFormat,
    destinationDirectory: Path,
    renameRootFolder: Option[Path]
  ): TaskProgress[Unit] = {
    val taskProgress = new TaskProgressImplementation[Unit]

    def runExtraction(): Unit = {
      val rewritePath: Path => Path = renameRootFolder match {
        case Some(value) => new RootRenamer(value)
        case None        => identity[Path]
      }

      Logger.debug(s"Extracting $archivePath to $destinationDirectory")
      var missingPermissions: Int = 0

      val result = withOpenArchive(archivePath, format) { (archive, progress) =>
        Logger.debug(s"Opened $archivePath with format $format - $archive")
        for (entry <- ArchiveIterator(archive)) {
          Logger.debug("Henlo")
          Logger.debug(s"Seeing ${entry.getName}")
          if (!archive.canReadEntryData(entry)) {
            throw new RuntimeException(
              s"Cannot read ${entry.getName} from $archivePath. " +
              s"The archive may be corrupted."
            )
          } else {
            val path = parseArchiveEntryName(entry.getName)
            val destinationPath =
              destinationDirectory.resolve(rewritePath(path))
            if (entry.isDirectory) {
              Files.createDirectories(destinationPath)
            } else {
              val parent = destinationPath.getParent
              Files.createDirectories(parent)
              Using(Files.newOutputStream(destinationPath)) { out =>
                IOUtils.copy(archive, out)
              }
            }

            if (OS.isPOSIX) {
              getMode(entry) match {
                case Some(mode) =>
                  val permissions = FileSystem.decodePOSIXPermissions(mode)
                  Files.setPosixFilePermissions(destinationPath, permissions)
                case None =>
                  missingPermissions += 1
              }
            }
          }

          taskProgress.reportProgress(
            progress.alreadyRead(),
            progress.total()
          )
        }
      }

      if (missingPermissions > 0) {
        Logger.warn(
          s"Could not find permissions for $missingPermissions files in " +
          s"archive `$archivePath`, some files may not have been marked as " +
          s"executable."
        )
      }

      taskProgress.setComplete(result)
    }
    val thread = new Thread(() => runExtraction())
    thread.start()
    taskProgress
  }

  def test(): Unit = {
    FileSystem.withTemporaryDirectory("enso-what") { dir =>
      val src =
        Path.of("/home/radeusgd/NBO/releasesci/override/enso-engine-0.1.0.zip")
      val tmp = dir.resolve("tmp.zip")
      FileSystem.copyFile(src, tmp)
      Archive
        .extractArchive(
          tmp,
          Path.of("./test_archive_dbg"),
          None
        )
        .waitForResult(true)
        .get
    }
  }

  private def getMode(entry: ArchiveEntry): Option[Int] =
    entry match {
      case entry: TarArchiveEntry =>
        Some(entry.getMode)
      case entry: ZipArchiveEntry =>
        if (entry.getPlatform == ZipArchiveEntry.PLATFORM_UNIX)
          Some(entry.getUnixMode)
        else None
      case _ =>
        None
    }

  def withOpenArchive[R](path: Path, format: ArchiveFormat)(
    action: (ArchiveInputStream, ReadProgress) => R
  ): Try[R] = {
    Logger.debug(s"Opening $path")
    Using(new FileProgressInputStream(path)) { progressInputStream =>
      Logger.debug(s"Created progress-reported stream $progressInputStream")
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

  /**
    * Parses the name of the entry inside of the archive into a relative path.
    *
    * The specification does not restrict entry names to valid paths, but
    * empirical testing shows that paths in the archives we need to support are
    * usually separated by '/' which is correctly parsed into a system-dependent
    * path both on UNIX and Windows platforms.
    */
  private def parseArchiveEntryName(name: String): Path =
    Path.of(name)

  trait ReadProgress {
    def alreadyRead(): Long
    def total():       Option[Long]
  }

}

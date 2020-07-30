package org.enso.launcher.archive

import java.io.BufferedInputStream
import java.nio.file.Path

import org.apache.commons.compress.archivers.ArchiveInputStream
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.enso.cli.{TaskProgress, TaskProgressImplementation}
import org.enso.launcher.Logger

import scala.util.{Try, Using}

object Archive {
  trait ReadProgress {
    def alreadyRead(): Long
    def total():       Long
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

      val result = withOpenArchive(archivePath, format) { (archive, progress) =>
        for (entry <- ArchiveIterator(archive)) {
          if (!archive.canReadEntryData(entry)) {
            Logger.warn(
              s"Cannot read ${entry.getName} from $archivePath. " +
              s"The archive may be corrupted."
            )
          } else {
            val path = parseArchiveEntryName(entry.getName)
            val destinationPath =
              destinationDirectory.resolve(rewritePath(path))
            Logger.debug(s"Extracting to $destinationPath")
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
}

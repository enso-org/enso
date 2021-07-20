package org.enso.librarymanager.published.repository

import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveOutputStream
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.Path
import scala.util.Using

object ArchiveWriter {
  sealed trait FileToWrite {
    def relativePath: String
  }

  case class TextFile(relativePath: String, content: String) extends FileToWrite

  def writeTarArchive(path: Path, files: Seq[FileToWrite]): Unit = {
    Using(new FileOutputStream(path.toFile)) { outputStream =>
      Using(new BufferedOutputStream(outputStream)) { bufferedStream =>
        Using(new GzipCompressorOutputStream(bufferedStream)) { gzipStream =>
          Using(new TarArchiveOutputStream(gzipStream)) { archive =>
            for (file <- files) {
              file match {
                case TextFile(relativePath, content) =>
                  val entry = new TarArchiveEntry(relativePath)
                  archive.putArchiveEntry(entry)
                  archive.write(content.getBytes)
                  archive.closeArchiveEntry()
              }
            }
          }
        }
      }
    }
  }
}

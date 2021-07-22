package org.enso.downloader.archive

import org.apache.commons.compress.archivers.tar.{
  TarArchiveEntry,
  TarArchiveOutputStream
}
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.{Try, Using}

class TarGzWriter private (archive: TarArchiveOutputStream) {
  def writeTextFile(relativePath: String, content: String): Unit = {
    val bytes = content.getBytes(StandardCharsets.UTF_8)
    val entry = new TarArchiveEntry(relativePath)
    entry.setSize(bytes.size.toLong)
    archive.putArchiveEntry(entry)
    archive.write(bytes)
    archive.closeArchiveEntry()
  }

  def writeFile(relativePath: String, filePath: Path): Long = {
    val entry = new TarArchiveEntry(filePath.toFile, relativePath)
    archive.putArchiveEntry(entry)
    val bytesWritten = Files.copy(filePath, archive)
    archive.closeArchiveEntry()
    bytesWritten
  }
}

object TarGzWriter {
  def createArchive(
    destination: Path
  )(actions: TarGzWriter => Unit): Try[Unit] =
    Using(new FileOutputStream(destination.toFile)) { outputStream =>
      Using(new BufferedOutputStream(outputStream)) { bufferedStream =>
        Using(new GzipCompressorOutputStream(bufferedStream)) { gzipStream =>
          Using(new TarArchiveOutputStream(gzipStream)) { archive =>
            val writer = new TarGzWriter(archive)
            actions(writer)
          }.get
        }.get
      }.get
    }
}

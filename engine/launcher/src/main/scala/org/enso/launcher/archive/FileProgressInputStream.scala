package org.enso.launcher.archive

import java.io.{FileInputStream, InputStream}
import java.nio.file.{Files, Path}

import org.enso.launcher.archive.Archive.ReadProgress

private[archive] class FileProgressInputStream(path: Path) extends InputStream {
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

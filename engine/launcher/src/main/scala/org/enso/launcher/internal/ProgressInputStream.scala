package org.enso.launcher.internal

import java.io.InputStream

trait ReadProgress {
  def alreadyRead(): Long
  def total():       Option[Long]
}

private[launcher] class ProgressInputStream(
  in: InputStream,
  totalSize: Option[Long],
  updated: ReadProgress => Unit
) extends InputStream {
  private var bytesRead: Long = 0

  private val readProgress = new ReadProgress {
    override def alreadyRead(): Long   = bytesRead
    override def total(): Option[Long] = totalSize
  }

  def progress: ReadProgress = readProgress

  override def available: Int =
    in.available()

  override def read: Int = {
    bytesRead += 1
    updated(readProgress)
    in.read()
  }

  override def read(b: Array[Byte]): Int = {
    val bytes = in.read(b)
    bytesRead += bytes
    updated(readProgress)
    bytes
  }

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    val bytes = in.read(b, off, len)
    bytesRead += bytes
    updated(readProgress)
    bytes
  }

  override def skip(n: Long): Long = {
    val skipped = in.skip(n)
    bytesRead += skipped
    updated(readProgress)
    skipped
  }

  override def close(): Unit =
    in.close()
}

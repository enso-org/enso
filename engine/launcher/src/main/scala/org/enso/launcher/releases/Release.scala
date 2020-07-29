package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.ProgressBar
import org.enso.cli.ProgressBar.TaskProgress

import scala.util.{Failure, Success, Try}

trait PendingDownload[A] extends TaskProgress[A] {
  def flatMap[B](f: A => Try[B]): PendingDownload[B] =
    new MappedDownload(this, f)
  def map[B](f: A => B): PendingDownload[B] = flatMap(a => Success(f(a)))
}

private class MappedDownload[A, B](source: PendingDownload[A], f: A => Try[B])
    extends PendingDownload[B] {
  override def addProgressListener(
    listener: ProgressBar.ProgressListener[B]
  ): Unit =
    source.addProgressListener(new ProgressBar.ProgressListener[A] {
      override def progressUpdate(done: Long, total: Option[Long]): Unit =
        listener.progressUpdate(done, total)
      override def done(result: Try[A]): Unit =
        listener.done(result.flatMap(f))
    })
}

object PendingDownload {
  def immediateFailure[A](throwable: Throwable): PendingDownload[A] =
    new PendingDownload[A] {
      override def addProgressListener(
        listener: ProgressBar.ProgressListener[A]
      ): Unit = {
        listener.done(Failure(throwable))
      }
    }
}

trait Asset {
  def fileName:               String
  def downloadTo(path: Path): PendingDownload[Unit]
  def fetchAsText():          PendingDownload[String]
}

trait Release {
  def tag:    String
  def assets: Seq[Asset]
}

trait ReleaseProvider {
  def releaseForVersion(tag: String): Try[Release]
  def listReleases():                 Try[Seq[Release]]
}

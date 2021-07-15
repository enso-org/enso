package org.enso.languageserver.libraries

import org.enso.cli.task.{
  ProgressReporter,
  ProgressUnit,
  TaskProgress,
  TaskProgressImplementation
}

import scala.util.Success

object FakeDownload {

  /** Creates a [[TaskProgress]] which reports progress updates for a few seconds.
    *
    * Intended for mocking download-like endpoints.
    */
  def make(seconds: Int = 10): TaskProgress[Unit] = {
    val tracker = new TaskProgressImplementation[Unit](ProgressUnit.Bytes)
    val thread = new Thread(() => {
      val n = (seconds * 10).toLong
      for (i <- 0L to n) {
        tracker.reportProgress(i, Some(n))
      }
      tracker.setComplete(Success(()))
    })
    thread.start()
    tracker
  }

  def simulateDownload(
    message: String,
    progressReporter: ProgressReporter
  ): Unit = {
    val download = make()
    progressReporter.trackProgress(message, download)
    download.force()
  }
}

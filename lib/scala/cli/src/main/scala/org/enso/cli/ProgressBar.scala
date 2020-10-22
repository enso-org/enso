package org.enso.cli

import java.util.concurrent.LinkedTransferQueue

import scala.util.Try

/** Allows to display a progress bar in a terminal.
  */
object ProgressBar {

  /** Displays a progressbar tracking progress of the provided task and waits
    * for its completion.
    *
    * The progress bar is displayed as long as the task is in progress. The
    * function blocks until the task is completed, and then it returns the
    * result of the task.
    *
    * If the total amount of work is unknown and progress cannot be estimated,
    * an in-progress animation is shown which is updated on each progress
    * update.
    */
  def waitWithProgress[A](task: TaskProgress[A]): Try[A] = {
    val progressBar = new internal.ProgressBar
    progressBar.start()

    sealed trait Update
    case class Progress(done: Long, total: Option[Long]) extends Update
    case class Done(result: Try[A])                      extends Update

    val queue = new LinkedTransferQueue[Update]()
    task.addProgressListener(new ProgressListener[A] {
      override def progressUpdate(done: Long, total: Option[Long]): Unit =
        queue.put(Progress(done, total))
      override def done(result: Try[A]): Unit =
        queue.put(Done(result))
    })

    var result: Option[Try[A]] = None
    while (result.isEmpty) {
      queue.take() match {
        case Progress(done, Some(total)) =>
          progressBar.updateProgress(100.0f * done / total)
        case Progress(_, None) =>
          progressBar.showUnknownProgress()
        case Done(incomingResult) =>
          progressBar.hide()

          result = Some(incomingResult)
      }
    }

    result.get
  }
}

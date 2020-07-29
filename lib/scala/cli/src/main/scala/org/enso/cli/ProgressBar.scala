package org.enso.cli

import java.util.concurrent.LinkedTransferQueue

import scala.util.Try

object ProgressBar {

  /**
    * Clients can implement this trait to get progress updates.
    */
  trait ProgressListener[A] {
    def progressUpdate(done: Long, total: Option[Long]): Unit
    def done(result: Try[A]):                            Unit
  }
  trait TaskProgress[A] {

    /**
      * Adds a progress listener to this task.
      *
      * Even if the task is already finished, the [[ProgressListener.done]]
      * method should be fired with the result. This way, `done` is fired
      * exactly once for each attached listener. There are no guarantees on how
      * often [[ProgressListener.progressUpdate]] is called.
      */
    def addProgressListener(listener: ProgressListener[A]): Unit

    def waitForResult(showProgress: Boolean = false): Try[A] =
      if (showProgress) waitWithProgress(this) else waitForTask(this)
  }

  def waitForTask[A](task: TaskProgress[A]): Try[A] = {
    val queue = new LinkedTransferQueue[Try[A]]()
    task.addProgressListener(new ProgressListener[A] {
      override def progressUpdate(done: Long, total: Option[Long]): Unit = {}
      override def done(result: Try[A]): Unit =
        queue.put(result)
    })

    queue.take()
  }

  def waitWithProgress[A](task: TaskProgress[A]): Try[A] = {
    startProgress()

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
    var unknownCounter         = 0
    while (result.isEmpty) {
      queue.take() match {
        case Progress(done, Some(total)) =>
          showProgress(100.0f * done / total)
        case Progress(_, None) =>
          unknownCounter += 1
          showUnknownProgress(unknownCounter)
        case Done(incomingResult) =>
          finishProgress(if (incomingResult.isSuccess) "done" else "failed")
          result = Some(incomingResult)
      }
    }

    result.get
  }

  def startProgress(): Unit = {
    drawProgressBar(0, "")
  }

  def showProgress(percentage: Float): Unit = {
    drawProgressBar(
      (percentage / 100.0f * totalStates).floor.toInt,
      s"${percentage.toInt}%"
    )
  }

  def showUnknownProgress(state: Int): Unit = {
    val pos    = state % progressWidth
    val prefix = " " * pos
    val suffix = " " * (progressWidth - pos - 1)
    val bar    = s"[$prefix?$suffix]\r"
    print(bar)
  }

  def finishProgress(comment: String): Unit = {
    drawProgressBar(totalStates, comment)
    println()
  }

  private val progressWidth  = 20
  private val progressStates = Seq(".", "#")
  private val totalStates    = progressWidth * progressStates.length

  private def drawProgressBar(state: Int, comment: String): Unit = {
    val full = progressStates.last * (state / progressStates.length)
    val partial = {
      val idx = state % progressStates.length
      if (idx > 0) {
        progressStates(idx - 1)
      } else ""
    }

    val bar     = full + partial
    val rest    = " " * (progressWidth - bar.length)
    val padding = " " * 5
    val line    = s"[$bar$rest] $comment$padding\r"
    print(line)
  }

  def simulate(): Unit = {
    startProgress()
    for (i <- 1 to 100) {
      showUnknownProgress(i)
      Thread.sleep(100)
    }
    finishProgress("hmm")
  }
}

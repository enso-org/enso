package org.enso.cli

import java.util.concurrent.LinkedTransferQueue

import scala.util.Try

object ProgressBar {

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
    val stateClamped = Seq(Seq(state, 0).max, totalStates).min
    val full =
      progressStates.last * (stateClamped / progressStates.length)
    val partial = {
      val idx = stateClamped % progressStates.length
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
}

package org.enso.cli

import java.util.concurrent.LinkedTransferQueue

import scala.util.{Failure, Success, Try}

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
    if (showProgress) ProgressBar.waitWithProgress(this)
    else TaskProgress.waitForTask(this)

  def flatMap[B](f: A => Try[B]): TaskProgress[B] =
    new MappedTask(this, f)
  def map[B](f: A => B): TaskProgress[B] = flatMap(a => Success(f(a)))
}

object TaskProgress {
  def immediateFailure[A](throwable: Throwable): TaskProgress[A] =
    new TaskProgress[A] {
      override def addProgressListener(
        listener: ProgressListener[A]
      ): Unit = {
        listener.done(Failure(throwable))
      }
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
}

private class MappedTask[A, B](source: TaskProgress[A], f: A => Try[B])
    extends TaskProgress[B] {
  override def addProgressListener(
    listener: ProgressListener[B]
  ): Unit =
    source.addProgressListener(new ProgressListener[A] {
      override def progressUpdate(done: Long, total: Option[Long]): Unit =
        listener.progressUpdate(done, total)
      override def done(result: Try[A]): Unit =
        listener.done(result.flatMap(f))
    })
}

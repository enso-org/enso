package org.enso.cli

import java.util.concurrent.LinkedTransferQueue

import scala.util.{Failure, Try}

/** Clients can implement this trait to get progress updates.
  */
trait ProgressListener[A] {
  def progressUpdate(done: Long, total: Option[Long]): Unit
  def done(result: Try[A]):                            Unit
}

/** Represents a long-running background task.
  */
trait TaskProgress[A] {

  /** Adds a progress listener to this task.
    *
    * Even if the task is already finished, the [[ProgressListener.done]]
    * method should be fired with the result. This way, `done` is fired
    * exactly once for each attached listener. There are no guarantees on how
    * often [[ProgressListener.progressUpdate]] is called.
    */
  def addProgressListener(listener: ProgressListener[A]): Unit

  /** Blocks and waits for the completion of the task.
    *
    * Optionally displays a progress bar in the terminal. Returns a [[Try]]
    * value that wraps the result.
    *
    * @param showProgress whether or not to show a progress bar while waiting
    */
  def waitForResult(showProgress: Boolean = false): Try[A] =
    if (showProgress) ProgressBar.waitWithProgress(this)
    else TaskProgress.waitForTask(this)

  /** Alters the task by transforming its result with a function `f` that may
    * fail.
    *
    * The progress of applying `f` is not monitored - it is meant for functions
    * that do not take a very long time in comparison with the base task.
    *
    * @param f the function that can transform the original result
    * @tparam B the type that `f` returns wrapped in a [[Try]]
    * @return a new [[TaskProgress]] that will succeed if the original one
    *         succeeded and the transformation succeeded too
    */
  def flatMap[B](f: A => Try[B]): TaskProgress[B] =
    new MappedTask(this, f)

  /** Alters the task by transforming its result with a function `f`.
    *
    * The progress of applying `f` is not monitored - it is meant for functions
    * that do not take a very long time in comparison with the base task.
    *
    * If an exception is thrown by `f`, the altered task returns a [[Failure]]
    * with that exception.
    *
    * @param f the function that can transform the original result
    * @tparam B resulting type of `f`
    * @return a new [[TaskProgress]] that will succeed if the original one
    *         succeeded and the transformation succeeded too
    */
  def map[B](f: A => B): TaskProgress[B] = flatMap(a => Try(f(a)))
}

object TaskProgress {

  /** Creates a task that fails immediately.
    *
    * Useful for reporting early errors (before a background thread has even
    * been started for the task).
    *
    * @param throwable the error to complete the task with
    * @tparam A type of the task that is failed
    */
  def immediateFailure[A](throwable: Throwable): TaskProgress[A] =
    new TaskProgress[A] {
      override def addProgressListener(
        listener: ProgressListener[A]
      ): Unit = {
        listener.done(Failure(throwable))
      }
    }

  /** Blocks and waits for the task to complete.
    */
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

/** Transforms the result of the `source` task with `f`.
  *
  * Used internally by [[TaskProgress.flatMap]].
  */
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

/** A simple implementation of [[TaskProgress]] that can be used to report
  * progress updates and mark completion of a task.
  */
class TaskProgressImplementation[A] extends TaskProgress[A] {
  @volatile private var listeners: List[ProgressListener[A]] = Nil
  private var result: Option[Try[A]]                         = None

  override def addProgressListener(
    listener: ProgressListener[A]
  ): Unit = {
    this.synchronized {
      result match {
        case Some(value) =>
          listener.done(value)
        case None =>
      }

      listeners ::= listener
    }
  }

  /** Marks the completion of this task.
    *
    * All registered listeners are immediately notified. Any listeners added
    * later will also be notified as soon as they are added.
    *
    * Can be called only once per task.
    * @param result the result to complete the task with
    */
  def setComplete(result: Try[A]): Unit = {
    this.synchronized {
      if (this.result.isDefined) {
        throw new IllegalStateException(
          "A task has been completed more than once."
        )
      }

      this.result = Some(result)
      listeners.foreach(_.done(result))
    }
  }

  /** Report a progress update to all registered listeners.
    *
    * This operation is not synchronized, as it is not a problem if a just added
    * listener does not get the latest progress update.
    */
  def reportProgress(done: Long, total: Option[Long]): Unit = {
    listeners.foreach(_.progressUpdate(done, total))
  }
}

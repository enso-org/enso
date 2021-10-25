package org.enso.cli.task

import scala.util.Try

/** A simple implementation of [[TaskProgress]] that can be used to report
  * progress updates and mark completion of a task.
  */
class TaskProgressImplementation[A](
  override val unit: ProgressUnit = ProgressUnit.Unspecified
) extends TaskProgress[A] {
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
    *
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

package org.enso.cli.task

import scala.util.Try

/** Clients can implement this trait to get progress updates.
  */
trait ProgressListener[-A] {

  /** Called whenever there is progress of the task.
    *
    * @param done represents how much progress has been done
    * @param total represents how much work is expected in total, if known
    */
  def progressUpdate(done: Long, total: Option[Long]): Unit

  /** Called when the task is finished.
    *
    * Even if a listener is added after the tas has already been finished, this
    * function should be called.
    *
    * @param result represents the result of the task - contains either a
    *               successfully computed value or a thrown exception
    */
  def done(result: Try[A]): Unit
}

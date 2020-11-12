package org.enso.cli.task

import scala.util.Try

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

  override def unit: ProgressUnit = source.unit
}

package org.enso.cli.task

import scala.util.Try

/** Transforms the result of the `source` task with `f`.
  *
  * Used internally by [[TaskProgress.flatMap]].
  */
private class MappedTask[A, B](source: TaskProgress[A], f: A => Try[B])
    extends TaskProgress[B] { self =>

  var listeners: List[ProgressListener[B]] = Nil
  var savedResult: Option[Try[B]]          = None

  source.addProgressListener(new ProgressListener[A] {
    override def progressUpdate(done: Long, total: Option[Long]): Unit =
      listeners.foreach(_.progressUpdate(done, total))

    override def done(result: Try[A]): Unit = self.synchronized {
      val mapped = result.flatMap(f)
      savedResult = Some(mapped)
      listeners.foreach(_.done(mapped))
    }
  })

  override def addProgressListener(listener: ProgressListener[B]): Unit =
    self.synchronized {
      listeners ::= listener
      savedResult match {
        case Some(saved) => listener.done(saved)
        case None        =>
      }
    }

  override def unit: ProgressUnit = source.unit
}

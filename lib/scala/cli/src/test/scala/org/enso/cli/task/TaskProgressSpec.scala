package org.enso.cli.task

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.util.{Success, Try}

class TaskProgressSpec extends AsyncWordSpec with Matchers {
  "TaskProgress.map" should {
    "run only once even with multiple listeners" in {
      var runs  = 0
      val task1 = new TaskProgressImplementation[String]()
      val task2 = task1.map { str =>
        runs += 1
        str + "bar"
      }

      val emptyListener = new ProgressListener[String] {
        override def progressUpdate(done: Long, total: Option[Long]): Unit = ()
        override def done(result: Try[String]): Unit                       = ()
      }
      task2.addProgressListener(emptyListener)
      task2.addProgressListener(emptyListener)

      task1.setComplete(Success("foo"))

      task2.addProgressListener(emptyListener)
      var answer: Option[Try[String]] = None
      task2.addProgressListener(new ProgressListener[String] {
        override def progressUpdate(done: Long, total: Option[Long]): Unit = ()
        override def done(result: Try[String]): Unit = {
          answer = Some(result)
        }
      })

      answer shouldEqual Some(Success("foobar"))
      runs shouldEqual 1
    }
  }

  "TaskProgress.toFuture" should {
    "return a future that is completed when the original task is" in {
      val task1 = new TaskProgressImplementation[String]()
      task1.setComplete(Success("foo"))

      task1.toFuture.map { result =>
        result shouldEqual "foo"
      }
    }
  }
}

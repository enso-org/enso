package org.enso.testkit

import org.scalatest._

/** Trait is used to retry the marked test in case of failure. */
trait RetrySpec extends TestSuite {

  /** Tag the test to be retried a specified number of times until success.
    *
    * @param times the number of attempted retries
    */
  case class Retry(times: Int) extends Tag(Retry.tagName(times)) {
    assert(times > 0, "number of retries should be a positive number")
  }

  case object Retry {

    /** Retry the test a single time. */
    def apply(): Retry =
      new Retry(1)

    val Name      = "org.enso.test.retry"
    val Separator = "-"

    /** Create the tag name. */
    def tagName(n: Int): String =
      s"$Name$Separator$n"

    /** Parse the number of retries from the tag name. */
    def parseRetries(tag: String): Int =
      tag.drop(Name.length + Separator.length).toInt
  }

  override def withFixture(test: NoArgTest): Outcome = {
    @scala.annotation.tailrec
    def go(n: Int, outcomes: List[Outcome]): Outcome =
      if (n > 0) {
        val result = super.withFixture(test)
        result match {
          case Failed(_) | Canceled(_) =>
            go(n - 1, result :: outcomes)
          case outcome => outcome
        }
      } else outcomes.head

    test.tags.find(_.contains(Retry.Name)) match {
      case Some(tag) =>
        go(Retry.parseRetries(tag) + 1, Nil)
      case None =>
        super.withFixture(test)
    }
  }

}

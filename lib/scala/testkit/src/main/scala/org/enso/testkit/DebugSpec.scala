package org.enso.testkit

import org.enso.runtime.utils.ThreadUtils
import org.scalatest._

/** Trait provides debug information when a test fails in the suite. */
trait DebugSpec extends TestSuite {

  override def withFixture(test: NoArgTest): Outcome = {
    val result = super.withFixture(test)

    if (result.isFailed || result.isCanceled) {
      val msg = ThreadUtils.dumpAllStacktraces(
        s"Thread dump of the failed test `${test.name}`"
      )
      println(msg)
    }

    result
  }
}

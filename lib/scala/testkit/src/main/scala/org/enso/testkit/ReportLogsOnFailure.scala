package org.enso.testkit

import org.scalatest.{Args, Failed, Outcome, Status, TestSuite}
import org.enso.logging.service.logback.test.TestAppenderUtils

trait ReportLogsOnFailure extends TestSuite {

  abstract override protected def runTest(
    testName: String,
    args: Args
  ): Status = {
    val appender = TestAppenderUtils.getMemoryAppender
    if (appender != null) {
      try {
        super.runTest(testName, args)
      } catch {
        case e: Throwable =>
          appender.flush()
          throw e
      }
    } else {
      super.runTest(testName, args)
    }
  }

  abstract override def withFixture(test: NoArgTest): Outcome = {
    val appender = TestAppenderUtils.getMemoryAppender()
    if (appender != null) {
      try {
        super.withFixture(test) match {
          case outcome @ Failed(_) =>
            appender.flush()
            outcome
          case outcome =>
            outcome
        }
      } finally {
        appender.reset()
      }
    } else {
      super.withFixture(test)
    }
  }

}

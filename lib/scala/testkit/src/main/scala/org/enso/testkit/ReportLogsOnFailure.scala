package org.enso.testkit

import org.scalatest.{Args, Failed, Outcome, Status, TestSuite}
import org.enso.logger.ObservedMessage

trait ReportLogsOnFailure extends TestSuite {

  abstract override protected def runTest(
    testName: String,
    args: Args
  ): Status = {
    val log    = org.slf4j.LoggerFactory.getLogger("org")
    val ps     = System.out
    val arr    = new java.util.ArrayList[ObservedMessage]()
    val handle = ObservedMessage.observe(log, arr.add(_))
    try {
      super.runTest(testName, args)
    } catch {
      case e: Throwable =>
        arr.forEach {
          ps.println(_)
        }
        throw e
    } finally {
      arr.clear()
      handle.close()
    }
  }

  abstract override def withFixture(test: NoArgTest): Outcome = {
    val log    = org.slf4j.LoggerFactory.getLogger("org")
    val ps     = System.out
    val arr    = new java.util.ArrayList[ObservedMessage]()
    val handle = ObservedMessage.observe(log, arr.add(_))
    try {
      super.withFixture(test) match {
        case outcome @ Failed(_) =>
          arr.forEach {
            ps.println(_)
          }
          outcome
        case outcome =>
          outcome
      }
    } finally {
      arr.clear()
      handle.close()
    }
  }

}

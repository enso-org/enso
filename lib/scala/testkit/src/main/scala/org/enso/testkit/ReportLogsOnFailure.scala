package org.enso.testkit

import org.scalatest.{Failed, Outcome, TestSuite}
import org.enso.logger.ObservedMessage
import org.slf4j.LoggerFactory
import org.slf4j.Logger

trait ReportLogsOnFailure extends TestSuite {

  abstract override def withFixture(test: NoArgTest): Outcome = {
    val log    = LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME)
    val arr    = new java.util.ArrayList[ObservedMessage]()
    val handle = ObservedMessage.observe(log, arr.add(_))
    try {
      super.withFixture(test) match {
        case outcome @ Failed(_) =>
          arr.forEach {
            System.out.println(_)
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

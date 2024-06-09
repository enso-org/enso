package org.enso.logger

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.Appender
import org.scalatest.{Args, Failed, Outcome, Status, TestSuite}
import org.slf4j.{Logger, LoggerFactory}

trait ReportLogsOnFailure extends TestSuite {

  private lazy val appender: Appender[ILoggingEvent] = {
    val ctx = LoggerFactory.getILoggerFactory;
    val rootLogger = ctx
      .getLogger(Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[ch.qos.logback.classic.Logger]
    rootLogger.getAppender(MemoryAppender.NAME)
  }

  abstract override protected def runTest(
    testName: String,
    args: Args
  ): Status = {
    appender match {
      case memoryAppender: MemoryAppender =>
        memoryAppender.stopForwarding()
        super.runTest(testName, args)
      case _ =>
        super.runTest(testName, args)
    }

  }

  abstract override def withFixture(test: NoArgTest): Outcome = {
    appender match {
      case memoryAppender: MemoryAppender =>
        try {
          super.withFixture(test) match {
            case outcome @ Failed(_) =>
              memoryAppender.flush()
              outcome
            case outcome =>
              outcome
          }
        } finally {
          memoryAppender.reset()
        }
      case _ =>
        super.withFixture(test)
    }
  }

}

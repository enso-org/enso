package org.enso.logger

import org.slf4j.LoggerFactory
import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.LoggerContext

object TestLogger {

  def gather[T, A](of: Class[A], action: => T): (T, List[TestLogMessage]) = {
    val logger = LoggerFactory.getLogger(of).asInstanceOf[Logger]
    //val (_, logs) = TestLogger.gatherLogs {
    val appender = new TestAppender()
    appender.setContext(
      LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
    );

    logger.addAppender(appender)
    appender.start()
    val result = action
    (result, appender.allEvents())
  }

}

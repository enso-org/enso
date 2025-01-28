package org.enso.logger.test

import org.slf4j.LoggerFactory
import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.core.read.ListAppender
import ch.qos.logback.classic.spi.ILoggingEvent

import scala.jdk.CollectionConverters._

object TestLogger
    extends java.util.function.BiFunction[
      org.slf4j.Logger,
      Runnable,
      java.util.List[TestLogMessage]
    ] {

  /** Gathers all logs of a specified type while executing a closure.
    *
    * @param of class of logs to collect
    * @param action a generic closure to execute
    * @tparam T the return type of executing a closure
    * @tparam A the type of logs to collect
    * @return a tuple with the result of executing the closure and the list of log events collected
    */
  def gather[T, A](of: Class[A], action: => T): (T, List[TestLogMessage]) = {
    val logger = LoggerFactory.getLogger(of).asInstanceOf[Logger]
    gatherLogs(logger, Level.WARN, action)
  }

  /** Capture the log messages.
    *
    * @param logger logger to observe
    * @return the messages
    */
  def apply(
    logger: org.slf4j.Logger,
    action: Runnable
  ): java.util.List[TestLogMessage] = {
    val msgs = gatherLogs(
      logger.asInstanceOf[Logger],
      Level.TRACE, {
        action.run()
      }
    )._2
    msgs.asJava
  }

  private def gatherLogs[T](
    logger: Logger,
    level: Level,
    action: => T
  ): (T, List[TestLogMessage]) = {
    val appender = new TestAppender()
    appender.setContext(
      LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
    )
    logger.setLevel(level)
    logger.addAppender(appender)
    appender.start()
    val result = action
    (result, appender.allEvents())
  }

  private class TestAppender extends ListAppender[ILoggingEvent] {

    def size(): Int = {
      this.list.size();
    }

    def allEvents(): List[TestLogMessage] = {
      this.list.asScala.toList.map(event =>
        TestLogMessage(event.getLevel(), event.getFormattedMessage())
      )
    }
  }

}

package org.enso.logger

import org.slf4j.LoggerFactory
import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.LoggerContext

object TestLogger {

  /** Gathers all logs of a specified type while executing a closure.
    *
    * @param of class of logs to collect
    * @param action a generic closure to execute
    * @tparam T the return type of executing a closure
    * @tparam A the type of logs to collect
    * @return a tuple with the result of executing the closure and the list of log events collected
    */
  def gather[T, A](of: Class[A], action: => T): (T, List[TestLogMessage]) = {
    val logger   = LoggerFactory.getLogger(of).asInstanceOf[Logger]
    val appender = new TestAppender()
    appender.setContext(
      LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
    )
    logger.addAppender(appender)
    appender.start()
    val result = action
    (result, appender.allEvents())
  }

}

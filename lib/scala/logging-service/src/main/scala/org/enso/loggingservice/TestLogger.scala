package org.enso.loggingservice

import org.enso.loggingservice.printers.TestPrinter

/** A helper object for handling logs in tests.
  */
object TestLogger {

  /** A log message returned by [[gatherLogs]].
    *
    * It contains the loglevel and message, but ignores attached exceptions.
    */
  case class TestLogMessage(logLevel: LogLevel, message: String)

  /** Gathers logs logged during execution of `action`.
    *
    * This method should be used only inside of tests. Any tests using it should
    * be ran with `parallelExecution` set to false, as global logger state has
    * to be modified to gather the logs.
    */
  def gatherLogs(action: => Unit): Seq[TestLogMessage] = {
    LoggingServiceManager.dropPendingLogs()
    LoggingServiceManager.tearDown()
    val printer = new TestPrinter
    LoggingServiceManager.setup(
      LoggerMode.Local(Seq(printer)),
      LogLevel.Debug
    )
    action
    LoggingServiceManager.tearDown()
    printer.getLoggedMessages
  }

  /** Drops any logs that are pending due to the logging service not being set
    * up.
    *
    * This method should be used only inside of tests. Any tests using it should
    * be ran with `parallelExecution` set to false, as global logger state has
    * to be modified to gather the logs.
    */
  def dropLogs(): Unit = {
    LoggingServiceManager.dropPendingLogs()
  }
}

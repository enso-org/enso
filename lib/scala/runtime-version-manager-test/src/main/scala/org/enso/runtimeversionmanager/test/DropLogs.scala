package org.enso.runtimeversionmanager.test

import org.enso.loggingservice.TestLogger
import org.scalatest.{BeforeAndAfterAll, Suite}

/** Ensures that any pending logs that were not processed when executing tests
  * are ignored instead of being dumped to stderr.
  *
  * This does not affect tests that set up the loggings service themselves or
  * capture the logs. It only affects logs that were not handled at all.
  */
trait DropLogs extends BeforeAndAfterAll { self: Suite =>
  override def afterAll(): Unit = {
    super.afterAll()
    TestLogger.dropLogs()
  }
}

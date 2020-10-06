package org.enso.launcher

import org.enso.loggingservice.TestLogger
import org.scalatest.{BeforeAndAfterAll, Suite}

trait DropLogs extends BeforeAndAfterAll { self: Suite =>
  override protected def afterAll(): Unit = {
    super.afterAll()
    TestLogger.dropLogs()
  }
}

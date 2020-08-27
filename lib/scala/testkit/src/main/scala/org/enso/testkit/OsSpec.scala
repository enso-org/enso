package org.enso.testkit

import org.apache.commons.lang3.SystemUtils
import org.scalatest._

/** Trait is used to mark the test to run on specific platforms. */
trait OsSpec extends TestSuite {

  /** Tag test to run on Unix platforms. */
  object OsUnix extends Tag("org.enso.test.os.unix")

  /** Tag test to run on Windows platforms. */
  object OsWindows extends Tag("org.enso.test.os.windows")

  override def withFixture(test: NoArgTest): Outcome = {
    if (test.tags.contains(OsUnix.name)) {
      if (SystemUtils.IS_OS_UNIX) super.withFixture(test) else Pending
    } else if (test.tags.contains(OsWindows.name)) {
      if (SystemUtils.IS_OS_WINDOWS) super.withFixture(test) else Pending
    } else {
      super.withFixture(test)
    }
  }

}

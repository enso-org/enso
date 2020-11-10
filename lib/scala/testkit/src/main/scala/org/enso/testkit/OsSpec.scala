package org.enso.testkit

import org.apache.commons.lang3.SystemUtils
import org.scalatest._

/** Trait is used to mark the test to run on specific platforms. */
trait OsSpec extends TestSuite {

  /** Tag test to run on Unix platforms. */
  object OsUnix extends Tag("org.enso.test.os.unix")

  /** Tag test to run on Windows platforms. */
  object OsWindows extends Tag("org.enso.test.os.windows")

  /** Tag test to run on Linux platforms. */
  object OsLinux extends Tag("org.enso.test.os.linux")

  override def withFixture(test: NoArgTest): Outcome = {
    if (test.tags.contains(OsUnix.name)) {
      if (isUnix) super.withFixture(test) else Pending
    } else if (test.tags.contains(OsWindows.name)) {
      if (isWindows) super.withFixture(test) else Pending
    } else if (test.tags.contains(OsLinux.name)) {
      if (isLinux) super.withFixture(test) else Pending
    } else {
      super.withFixture(test)
    }
  }

  /** @return `true` if this is a Unix-like system. */
  protected def isLinux: Boolean =
    SystemUtils.IS_OS_LINUX

  /** @return `true` if this is a Unix-like system. */
  protected def isUnix: Boolean =
    SystemUtils.IS_OS_UNIX

  /** @return `true` if this is a Windows system. */
  protected def isWindows: Boolean =
    SystemUtils.IS_OS_WINDOWS

}

package org.enso.runtimeversionmanager.test

import org.enso.testkit.TestSynchronizer

class SlowTestSynchronizer extends TestSynchronizer {
  override val timeOutSeconds: Long = 90
}

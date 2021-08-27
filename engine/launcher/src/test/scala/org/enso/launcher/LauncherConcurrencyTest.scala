package org.enso.launcher

import org.enso.launcher.distribution.LauncherResourceManager
import org.enso.runtimeversionmanager.test.{
  FakeEnvironment,
  SlowTestSynchronizer,
  TestLocalLockManager
}
import org.enso.testkit.{FlakySpec, RetrySpec, WithTemporaryDirectory}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpec

class LauncherConcurrencyTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with FakeEnvironment
    with BeforeAndAfterEach
    with TimeLimitedTests
    with RetrySpec
    with FlakySpec {

  /** This is an upper bound to avoid stalling the tests forever, but particular
    * operations have smaller timeouts usually.
    */
  val timeLimit: Span = 240.seconds

  "synchronize main lock" taggedAs Retry in {

    /** First two threads start and acquire the shared lock, than the third
      * thread tries to acquire an exclusive lock (in practice that will be our
      * (un)installer), it should wait for the other threads to finish. When
      * the threads see that it started waiting (the waiting notification is
      * normally used to tell the user what the application is waiting for),
      * the two threads finish and after that the third one is able to acquire
      * the exclusive lock.
      */
    val sync        = new SlowTestSynchronizer
    val lockManager = new TestLocalLockManager
    def makeNewResourceManager(): LauncherResourceManager =
      new LauncherResourceManager(lockManager)

    sync.startThread("t1") {
      val resourceManager = makeNewResourceManager()
      resourceManager.initializeMainLock()
      sync.report("shared-start")
      sync.signal("started-1")
      sync.waitFor("finish-1")
      sync.report("shared-end")
      resourceManager.releaseMainLock()
    }

    sync.startThread("t2") {
      val resourceManager = makeNewResourceManager()
      resourceManager.initializeMainLock()
      sync.report("shared-start")
      sync.signal("started-2")
      sync.waitFor("finish-2")
      sync.report("shared-end")
      resourceManager.releaseMainLock()
    }

    sync.waitFor("started-1")
    sync.waitFor("started-2")

    sync.startThread("t3") {
      val resourceManager = makeNewResourceManager()
      resourceManager.initializeMainLock()
      sync.report("t3-start")
      resourceManager.acquireExclusiveMainLock(() => {
        sync.report("t3-wait")
        sync.signal("waiting")
      })
      sync.report("t3-end")
      sync.signal("finish-all")
      resourceManager.releaseMainLock()
    }

    sync.waitFor("waiting")
    Thread.sleep(1000)

    sync.signal("finish-1")
    sync.signal("finish-2")

    sync.waitFor("finish-all")

    sync.join()
    sync.summarizeReports() shouldEqual Seq(
      "shared-start",
      "shared-start",
      "t3-start",
      "t3-wait",
      "shared-end",
      "shared-end",
      "t3-end"
    )
  }
}

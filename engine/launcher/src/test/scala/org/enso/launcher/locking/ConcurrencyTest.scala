package org.enso.launcher.locking

import org.enso.launcher.components.ComponentsManager
import org.enso.launcher.{FakeEnvironment, WithTemporaryDirectory}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConcurrencyTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with FakeEnvironment {

  def makeComponentsManager(releaseCallback: () => Unit): ComponentsManager = {
    ???
  }

  "locks" should {
    "synchronize parallel installations with the same runtime" ignore {}

    "synchronize installation and run" ignore {}

    "synchronize uninstallation and run" ignore {}

    "synchronize upgrades" ignore {}

    "synchronize main lock" in {
      val sync = new TestSynchronizer
      val t1 = new Thread(() => {
        val resourceManager = new ResourceManager(TestLocalLockManager)
        resourceManager.initializeMainLock()
        sync.report(1)
        sync.signal("started-1")
        sync.waitFor("finish-1")
        sync.report(4)
        resourceManager.releaseMainLock()
      })

      val t2 = new Thread(() => {
        val resourceManager = new ResourceManager(TestLocalLockManager)
        resourceManager.initializeMainLock()
        sync.report(1)
        sync.signal("started-2")
        sync.waitFor("finish-2")
        sync.report(4)
        resourceManager.releaseMainLock()
      })

      val t3 = new Thread(() => {
        val resourceManager = new ResourceManager(TestLocalLockManager)
        resourceManager.initializeMainLock()
        sync.report(2)
        resourceManager.acquireExclusiveMainLock(() => {
          sync.report(3)
          sync.signal("waiting")
        })
        sync.report(5)
        sync.signal("finish-all")
        resourceManager.releaseMainLock()
      })

      t1.start()
      t2.start()

      sync.waitFor("started-1")
      sync.waitFor("started-2")

      t3.start()
      sync.waitFor("waiting")
      Thread.sleep(1000)

      sync.signal("finish-1")
      sync.signal("finish-2")

      sync.waitFor("finish-all")

      sync.summarizeReports() shouldEqual Seq(1, 1, 2, 3, 4, 4, 5)
    }
  }
}

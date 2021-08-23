package org.enso.lockmanager

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.enso.distribution.locking.{LockManager, ThreadSafeFileLockManager}
import org.enso.lockmanager.client.ConnectedLockManager
import org.enso.lockmanager.server.LockManagerService
import org.enso.testkit.WithTemporaryDirectory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ConnectedLockManagerTest
    extends TestKit(ActorSystem("TestSystem"))
    with AnyWordSpecLike
    with Matchers
    with WithTemporaryDirectory {

  def lockRoot = getTestDirectory.resolve("locks")

  def setupLockManagers(): (LockManager, LockManager) = {
    val primaryLockManager = new ThreadSafeFileLockManager(lockRoot)

    val lockManagerService =
      system.actorOf(LockManagerService.props(primaryLockManager))

    val connectedLockManager = new ConnectedLockManager
    // TODO setup connection

    val _ = lockManagerService

    (primaryLockManager, connectedLockManager)
  }

  "ConnectedLockManager" should {
    "todo" in {
      val (primaryLockManager, connectedLockManager) = setupLockManagers()

      val _ = (primaryLockManager, connectedLockManager) // TODO
    }
  }
}

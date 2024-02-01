package org.enso.lockmanager

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.enso.distribution.locking.{
  LockManager,
  LockType,
  ThreadSafeFileLockManager
}
import org.enso.lockmanager.ActorToHandlerConnector.SetRequestHandler
import org.enso.lockmanager.client.{
  ConnectedLockManager,
  RuntimeServerRequestHandler
}
import org.enso.lockmanager.server.LockManagerService
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.{TestSynchronizer, WithTemporaryDirectory}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.util.Using

class ConnectedLockManagerTest
    extends TestKit(ActorSystem("TestSystem"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with WithTemporaryDirectory {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  private def lockRoot = getTestDirectory.resolve("locks")

  def setupLockManagers(): (LockManager, LockManager) = {
    val primaryLockManager = new ThreadSafeFileLockManager(lockRoot)

    val lockManagerService =
      system.actorOf(LockManagerService.props(primaryLockManager))

    val connector = system.actorOf(ActorToHandlerConnector.props())
    val endpoint = new RuntimeServerRequestHandler {
      override def sendToClient(request: Api.Request): Unit =
        lockManagerService.tell(request, sender = connector)
    }

    connector ! SetRequestHandler(endpoint)

    val connectedLockManager = new ConnectedLockManager
    connectedLockManager.connect(endpoint)

    (primaryLockManager, connectedLockManager)
  }

  private val resource = "test-resource"

  "ConnectedLockManager" should {
    "share exclusive locks between the connected lock manager and the primary one" in {
      val sync = new TestSynchronizer

      val (primary, connected) = setupLockManagers()

      sync.startThread("primary") {
        Using(
          primary.acquireLockWithWaitingAction(
            resource,
            LockType.Exclusive,
            () =>
              throw new RuntimeException(
                "First locker should not have to wait!"
              )
          )
        ) { _ =>
          sync.signal("primary-acquired")
          sync.report("primary-acquired")
          sync.waitFor("connected-is-waiting")
          sync.report("primary-releasing")
        }
      }

      sync.startThread("connected") {
        sync.waitFor("primary-acquired")
        Using(
          connected.acquireLockWithWaitingAction(
            resource,
            LockType.Exclusive,
            () => {
              sync.report("connected-waiting")
              sync.signal("connected-is-waiting")
            }
          )
        ) { _ =>
          sync.report("connected-acquired")
        }
      }

      sync.join()
      sync.summarizeReports() shouldEqual Seq(
        "primary-acquired",
        "connected-waiting",
        "primary-releasing",
        "connected-acquired"
      )
    }

    "correctly handle shared locks" in {
      val sync = new TestSynchronizer

      val (primary, connected) = setupLockManagers()

      sync.startThread("primary") {
        Using(
          primary.acquireLockWithWaitingAction(
            resource,
            LockType.Shared,
            () =>
              throw new RuntimeException(
                "First locker should not have to wait!"
              )
          )
        ) { _ =>
          sync.signal("primary-acquired")
          sync.report("primary-acquired")
          sync.waitFor("connected-acquired")
        }

        sync.report("released")
      }

      sync.startThread("connected") {
        sync.waitFor("primary-acquired")
        Using(
          connected.acquireLockWithWaitingAction(
            resource,
            LockType.Shared,
            () =>
              throw new RuntimeException(
                "Second locker should not have to wait!"
              )
          )
        ) { _ =>
          sync.report("connected-acquired")
          sync.signal("connected-acquired")
        }

        sync.report("released")
      }

      sync.join()
      sync.summarizeReports() shouldEqual Seq(
        "primary-acquired",
        "connected-acquired",
        "released",
        "released"
      )
    }
  }
}

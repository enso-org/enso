package org.enso.runtimeversionmanager.locking

import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.TimeUnit

import org.enso.runtimeversionmanager.test.{
  TestSynchronizer,
  WithTemporaryDirectory
}
import org.enso.testkit.OsSpec
import org.scalatest.OptionValues
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpec

class ThreadSafeFileLockManagerTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with TimeLimitedTests
    with OptionValues
    with OsSpec {

  val timeLimit: Span = 30.seconds

  private var testLocalLockManager: Option[ThreadSafeFileLockManager] = None
  override def beforeEach(): Unit = {
    super.beforeEach()
    testLocalLockManager = Some(new ThreadSafeFileLockManager(getTestDirectory))
  }

  def lockManager: ThreadSafeFileLockManager = testLocalLockManager.getOrElse(
    throw new IllegalStateException("Test not initialized")
  )

  "ThreadSafeFileLockManager" should {
    "allow multiple concurrent shared locks" in {
      val name = "resource1"
      val r1   = lockManager.acquireLock(name, LockType.Shared)
      val r2   = lockManager.acquireLock(name, LockType.Shared)
      r1.release()
      r2.release()
    }

    "allow no other locks if an exclusive lock is held" in {
      val sync     = new TestSynchronizer
      val name     = "resource1"
      val mainLock = lockManager.acquireLock(name, LockType.Exclusive)
      sync.report("acquired main")

      sync.startThread("exclusive") {
        lockManager.tryAcquireLock(name, LockType.Exclusive) shouldEqual None
        sync.signal("exclusive-waited")
        val lock = lockManager.acquireLock(name, LockType.Exclusive)
        sync.report("acquired")
        lock.release()
      }

      sync.startThread("shared") {
        lockManager.tryAcquireLock(name, LockType.Shared) shouldEqual None
        sync.signal("shared-waited")
        val lock = lockManager.acquireLock(name, LockType.Shared)
        sync.report("acquired")
        lock.release()
      }

      sync.waitFor("exclusive-waited")
      sync.waitFor("shared-waited")

      sync.report("releasing main")
      mainLock.release()

      sync.join(5)
      sync.summarizeReports() shouldEqual Seq(
        "acquired main",
        "releasing main",
        "acquired",
        "acquired"
      )
    }

    "allow exclusive lock only when no shared locks are held" in {
      val sync    = new TestSynchronizer
      val name    = "resource1"
      val shared1 = lockManager.acquireLock(name, LockType.Shared)

      sync.startThread("exclusive") {
        lockManager.tryAcquireLock(name, LockType.Exclusive) shouldEqual None
        sync.signal("waited")
        val lock = lockManager.acquireLock(name, LockType.Exclusive)
        sync.report("acquired")
        lock.release()
      }

      sync.waitFor("waited")

      val shared2 = lockManager.acquireLock(name, LockType.Shared)
      lockManager.acquireLock(name, LockType.Shared).release()
      sync.report("releasing-some")
      shared1.release()

      sync.report("releasing-last-one")

      shared2.release()

      sync.join(5)
      sync.summarizeReports() shouldEqual Seq(
        "releasing-some",
        "releasing-last-one",
        "acquired"
      )

    }

    "immediately resolve failed shared tryAcquire" in {
      val name = "resource1"
      lockManager.tryAcquireLock(name, LockType.Exclusive).value
      lockManager.tryAcquireLock(name, LockType.Shared) shouldEqual None
    }

    "immediately resolve failed exclusive tryAcquire" in {
      val name = "resource1"
      lockManager.tryAcquireLock(name, LockType.Exclusive).value
      lockManager.tryAcquireLock(name, LockType.Exclusive) shouldEqual None
    }

    /** TODO [RW] document
      */
    "immediately fail try* if waiting for another process" taggedAs OsLinux in {
      val sync = new TestSynchronizer

      val name = "resource1"
      val lockFilePath = FileLockManager
        .lockPath(getTestDirectory, name)
        .toAbsolutePath
        .normalize
      println(lockFilePath)
      val otherProcess = {
        // FIXME [RW] flock does not have to work as it has a different
        //  underlying mechanism from Java's FileLock which uses fcntl
        val pb = new ProcessBuilder(
          "flock",
          "--shared",
          "--nonblock",
          lockFilePath.toString,
          "-c",
          "cat"
        )
        pb.redirectInput(Redirect.PIPE)
        println(pb.command())
        pb.start()
      }

      try {
        Thread.sleep(200) // we need to ensure that flock has started already

        otherProcess.getOutputStream.close()
        println(otherProcess.waitFor())

        sync.startThread("exclusive-waiting") {
          assert(
            otherProcess.isAlive,
            "Other process should have acquired lock successfully and be waiting"
          )
          println("tryin to acquire")
          withClue(
            "The exclusive lock should not be acquired immediately as the " +
            "other process is holding a shared lock."
          ) {
            lockManager.tryAcquireLock(
              name,
              LockType.Exclusive
            ) shouldEqual None
          }
          println("well we treid")
          sync.signal("waited")
          val lock = lockManager.acquireLock(name, LockType.Exclusive)
          sync.report("exclusive-acquired")
          lock.release()
        }

        sync.waitFor("waited")
        Thread.sleep(200) // ensure that our thread has started waiting

        /** This would normally succeed, but the [[lockManager]] is busy waiting
          * for the exclusive lock, so it should fail.
          *
          * If it succeeds, that means that the sleep above was too short and that
          * the exclusive lock did not start waiting yet. Or alternatively, that
          * it was already acquired and released but that is later checked by
          * ensuring correct order of event reports.
          */
        lockManager.tryAcquireLock(name, LockType.Shared) shouldEqual None

        sync.report("try-acquire-was-busy")

        otherProcess.getOutputStream
          .close() // by closing the stream, we ensure that the `cat` finishes

        assert(
          otherProcess.waitFor(2, TimeUnit.SECONDS),
          "The flock process should terminate quickly"
        )
        withClue("The flock process should succeed") {
          otherProcess.waitFor() shouldEqual 0
        }

        sync.join()
        sync.summarizeReports() shouldEqual Seq(
          "try-acquire-was-busy",
          "exclusive-acquired"
        )
      } finally {
        otherProcess.destroy()
      }
    }

    "handle distinct resources independently" in {
      val r1 = lockManager.acquireLock("resource1", LockType.Shared)
      val r2 = lockManager.acquireLock("resource2", LockType.Shared)
      r1.release()
      r2.release()
    }
  }
}

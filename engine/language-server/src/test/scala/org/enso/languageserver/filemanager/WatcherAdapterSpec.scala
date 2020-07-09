package org.enso.languageserver.filemanager

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, LinkedBlockingQueue, Semaphore}

import org.apache.commons.io.FileUtils
import org.enso.languageserver.effect.Effects
import org.enso.testkit.RetrySpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.util.Try

class WatcherAdapterSpec
    extends AnyFlatSpec
    with Matchers
    with Effects
    with RetrySpec {

  import WatcherAdapter._

  final val Timeout: FiniteDuration = 5.seconds

  it should "get create events" taggedAs Retry() in withWatcher {
    (path, events) =>
      val fileA = Paths.get(path.toString, "a.txt")
      Files.createFile(fileA)
      val event = events.poll(Timeout.length, Timeout.unit)
      event shouldBe WatcherAdapter.WatcherEvent(fileA, EventTypeCreate)
  }

  it should "get delete events" taggedAs Retry() in withWatcher {
    (path, events) =>
      val fileA = Paths.get(path.toString, "a.txt")

      Files.createFile(fileA)
      val event1 = events.poll(Timeout.length, Timeout.unit)
      event1 shouldBe WatcherEvent(fileA, EventTypeCreate)

      Files.delete(fileA)
      val event2 = events.poll(Timeout.length, Timeout.unit)
      event2 shouldBe WatcherEvent(fileA, EventTypeDelete)
  }

  it should "get modify events" taggedAs Retry() in withWatcher {
    (path, events) =>
      val fileA = Paths.get(path.toString, "a.txt")

      Files.createFile(fileA)
      val event1 = events.poll(Timeout.length, Timeout.unit)
      event1 shouldBe WatcherEvent(fileA, EventTypeCreate)

      Files.write(fileA, "hello".getBytes())
      val event2 = events.poll(Timeout.length, Timeout.unit)
      event2 shouldBe WatcherEvent(fileA, EventTypeModify)
  }

  it should "get events from subdirectories" taggedAs Retry() in withWatcher {
    (path, events) =>
      val subdir = Paths.get(path.toString, "subdir")
      val fileA  = Paths.get(path.toString, "subdir", "a.txt")

      Files.createDirectories(subdir)
      val event1 = events.poll(Timeout.length, Timeout.unit)
      event1 shouldBe WatcherEvent(subdir, EventTypeCreate)

      Files.createFile(fileA)
      val event2 = events.poll(Timeout.length, Timeout.unit)
      event2 shouldBe WatcherEvent(fileA, EventTypeCreate)
  }

  def withWatcher(
    test: (Path, LinkedBlockingQueue[WatcherEvent]) => Any
  ): Any = {
    val lock     = new Semaphore(0)
    val executor = Executors.newSingleThreadExecutor()
    val tmp      = Files.createTempDirectory(null).toRealPath()
    val queue    = new LinkedBlockingQueue[WatcherEvent]()
    val watcher  = WatcherAdapter.build(tmp, queue.put, println(_))

    executor.submit { () =>
      lock.release()
      watcher.start().unsafeRunSync()
    }

    try {
      lock.tryAcquire(Timeout.length, Timeout.unit)
      test(tmp, queue)
    } finally {
      watcher.stop().unsafeRunSync()
      executor.shutdown()
      Try(executor.awaitTermination(Timeout.length, Timeout.unit))
      Try(FileUtils.deleteDirectory(tmp.toFile))
    }
  }
}

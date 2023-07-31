package org.enso.filewatcher

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, LinkedBlockingQueue, Semaphore}

import org.apache.commons.io.FileUtils
import org.enso.testkit.RetrySpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.util.Try

class WatcherAdapterSpec extends AnyFlatSpec with Matchers with RetrySpec {

  final val Timeout: FiniteDuration = 5.seconds

  it should "get create events" taggedAs Retry in withWatcher {
    (path, events) =>
      val fileA = Paths.get(path.toString, "a.txt")
      Files.createFile(fileA)
      val event = events.poll(Timeout.length, Timeout.unit)
      event shouldBe Watcher.WatcherEvent(fileA, Watcher.EventTypeCreate)
  }

  it should "get delete events" taggedAs Retry in withWatcher {
    (path, events) =>
      val fileA = Paths.get(path.toString, "a.txt")

      Files.createFile(fileA)
      val event1 = events.poll(Timeout.length, Timeout.unit)
      event1 shouldBe Watcher.WatcherEvent(fileA, Watcher.EventTypeCreate)

      Files.delete(fileA)
      val event2 = events.poll(Timeout.length, Timeout.unit)
      event2 shouldBe Watcher.WatcherEvent(fileA, Watcher.EventTypeDelete)
  }

  it should "get modify events" taggedAs Retry in withWatcher {
    (path, events) =>
      val fileA = Paths.get(path.toString, "a.txt")

      Files.createFile(fileA)
      val event1 = events.poll(Timeout.length, Timeout.unit)
      event1 shouldBe Watcher.WatcherEvent(fileA, Watcher.EventTypeCreate)

      Files.write(fileA, "hello".getBytes())
      val event2 = events.poll(Timeout.length, Timeout.unit)
      event2 shouldBe Watcher.WatcherEvent(fileA, Watcher.EventTypeModify)
  }

  it should "get events from subdirectories" taggedAs Retry in withWatcher {
    (path, events) =>
      val subdir = Paths.get(path.toString, "subdir")
      val fileA  = Paths.get(path.toString, "subdir", "a.txt")

      Files.createDirectories(subdir)
      val event1 = events.poll(Timeout.length, Timeout.unit)
      event1 shouldBe Watcher.WatcherEvent(subdir, Watcher.EventTypeCreate)

      Files.createFile(fileA)
      val event2 = events.poll(Timeout.length, Timeout.unit)
      event2 shouldBe Watcher.WatcherEvent(fileA, Watcher.EventTypeCreate)
  }

  def withWatcher(
    test: (Path, LinkedBlockingQueue[Watcher.WatcherEvent]) => Any
  ): Any = {
    val lock     = new Semaphore(0)
    val executor = Executors.newSingleThreadExecutor()
    val tmp      = Files.createTempDirectory(null).toRealPath()
    val queue    = new LinkedBlockingQueue[Watcher.WatcherEvent]()
    val watcher  = new WatcherAdapterFactory().build(tmp, queue.put, println(_))

    executor.submit[Any] { () =>
      lock.release()
      watcher.start()
    }

    try {
      lock.tryAcquire(Timeout.length, Timeout.unit)
      test(tmp, queue)
    } finally {
      watcher.stop()
      executor.shutdown()
      Try(executor.awaitTermination(Timeout.length, Timeout.unit))
      Try(FileUtils.deleteDirectory(tmp.toFile))
    }
  }
}

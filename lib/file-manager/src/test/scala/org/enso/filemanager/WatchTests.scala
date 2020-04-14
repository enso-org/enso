package org.enso.filemanager

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorRef
import akka.actor.typed.Scheduler
import akka.util.Timeout
import io.methvin.watcher.DirectoryChangeEvent
import java.nio.file.Files
import java.nio.file.NotDirectoryException
import java.nio.file.Path
import java.util.UUID

import org.apache.commons.io.FileUtils
import org.enso.FileManager
import org.scalatest.{BeforeAndAfterAll, Ignore, Outcome}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

// needs to be separate because watcher message are asynchronous
@Ignore
class WatchTests
    extends AnyFunSuite
    with BeforeAndAfterAll
    with Matchers
    with Helpers {
  import FileManager.API._

  var testKit: ActorTestKit         = ActorTestKit()
  implicit val timeout: Timeout     = 3.seconds
  implicit val scheduler: Scheduler = testKit.scheduler

  var fileManager: ActorRef[InputMessage]   = _
  var testProbe: TestProbe[FileSystemEvent] = _
  var watcherID: UUID                       = _

  override def withFixture(test: NoArgTest): Outcome = {
    withTemporaryDirectory(_ => {
      fileManager = testKit.spawn(FileManager(tempDir))
      testProbe   = testKit.createTestProbe[FileSystemEvent]("file-observer")
      watcherID   = observe(tempDir)

      try super.withFixture(test)
      finally if (watcherID != null)
        // Otherwise directory would stay blocked on Windows.
        unobserve(watcherID): Unit
    })
  }

  override def afterAll(): Unit = {
    testKit.shutdownTestKit()
  }

  def matchesEvent(
    path: Path,
    eventType: DirectoryChangeEvent.EventType
  )(message: FileSystemEvent): Boolean = {
    message.path == path && message.eventType == eventType
  }

  def expectEventFor(
    eventType: DirectoryChangeEvent.EventType,
    events: Seq[FileSystemEvent]
  )(path: Path): Unit = {
    assert(
      events.exists(matchesEvent(path, eventType)),
      s"not received message about $path"
    )
    ()
  }

  def expectNextEvent(
    path: Path,
    eventType: DirectoryChangeEvent.EventType,
    probe: TestProbe[FileSystemEvent] = testProbe
  ): Unit = {
    val message = probe.receiveMessage()
    assert(
      matchesEvent(path, eventType)(message),
      s"expected of type $eventType for $path, got $message"
    )
    ()
  }

  def ask[response <: Response.Success: ClassTag](
    requestPayload: Request.Payload[response]
  ): Future[Try[response]] = {
    FileManager.ask(fileManager, requestPayload)
  }

  def observe(
    path: Path,
    replyTo: ActorRef[FileSystemEvent] = testProbe.ref
  ): UUID = {
    val futureResponse = ask(Watch.Create.Request(path, replyTo))
    Await.result(futureResponse, timeout.duration).get.id
  }

  def unobserve(id: UUID): Watch.Remove.Response = {
    val futureResponse = ask(Watch.Remove.Request(id))
    Await.result(futureResponse, timeout.duration).get
  }

  // FIXME [MWU]
  //  This test has been temporarily disabled because it was failing from time
  //  to time on CI runs: https://github.com/luna/enso/issues/73
//  test("Watcher: observe subtree creation and deletion") {
//    val subtree = createSubtree()
//    val events  = testProbe.receiveMessages(subtree.elements.size)
//    subtree.elements.foreach(
//      expectEventFor(DirectoryChangeEvent.EventType.CREATE, events)
//    )
//
//    FileUtils.deleteDirectory(subtree.root.toFile)
//
//    val deletionEvents = testProbe.receiveMessages(subtree.elements.size)
//    subtree.elements.foreach(
//      expectEventFor(
//        DirectoryChangeEvent.EventType.DELETE,
//        deletionEvents
//      )
//    )
//
//    testProbe.expectNoMessage(50.millis)
//  }

  test("Watcher: observe file modification") {
    val dir10 = tempDir.resolve("dir10")
    Files.createDirectory(dir10)
    expectNextEvent(dir10, DirectoryChangeEvent.EventType.CREATE)

    val dir20 = dir10.resolve("dir20")
    Files.createDirectories(dir20)
    expectNextEvent(dir20, DirectoryChangeEvent.EventType.CREATE)

    val someFile = dir20.resolve("file.dat")
    Files.createFile(someFile)
    expectNextEvent(someFile, DirectoryChangeEvent.EventType.CREATE)

    // Need to wait a moment, as change soon after creation might be missed
    // otherwise by some subpar watch implementations.
    Thread.sleep(2000)
    Files.write(someFile, "blahblah".getBytes)
    expectNextEvent(someFile, DirectoryChangeEvent.EventType.MODIFY)

    Files.delete(someFile)
    expectNextEvent(someFile, DirectoryChangeEvent.EventType.DELETE)

    FileUtils.deleteDirectory(dir20.toFile)
    expectNextEvent(dir20, DirectoryChangeEvent.EventType.DELETE)
    testProbe.expectNoMessage(50.millis)
  }

  test("Watcher: disabling watch") {
    val subtree = createSubtree()
    testProbe.receiveMessages(subtree.elements.size)
    testProbe.expectNoMessage(50.millis)
    val stopResponse = unobserve(watcherID)
    watcherID = null
    stopResponse should be(Watch.Remove.Response())

    // Watch has been disabled, no further messages should come
    FileUtils.deleteDirectory(subtree.root.toFile)
    testProbe.expectNoMessage(50.millis)
  }

  test("Watcher: cannot watch ordinary file") {
    val file = createSubFile()
    assertThrows[NotDirectoryException]({ observe(file, testProbe.ref) })
  }

  test("Watcher: cannot watch symlink") {
    val dir     = createSubDir()
    val dirLink = tempDir.resolve("mylink")
    Files.createSymbolicLink(dirLink, dir)
    assertThrows[NotDirectoryException]({ observe(dirLink, testProbe.ref) })
  }

  test("Watcher: can watch under symlink") {
    // The observed directory is not and does not contain symlink,
    // however the path we observe it through contains symlink
    val top       = createSubDir()
    val linkToTop = top.resolve("link")
    Files.createSymbolicLink(linkToTop, top)

    val realSub = top.resolve("sub")
    val linkSub = linkToTop.resolve("sub")

    Files.createDirectory(realSub)

    val symlinkEventProbe =
      testKit.createTestProbe[FileSystemEvent]("observe-symlink-dir")

    val id = observe(linkSub, symlinkEventProbe.ref)
    try {
      // Create file using "real" path.
      val filename         = "testfile"
      val realFilePath     = realSub.resolve(filename)
      val observedFilePath = linkSub.resolve(filename)

      val expectedOfType = (eventType: DirectoryChangeEvent.EventType) =>
        expectNextEvent(
          observedFilePath,
          eventType,
          symlinkEventProbe
        )

      Files.createFile(realFilePath)
      expectedOfType(DirectoryChangeEvent.EventType.CREATE)

      Files.write(realFilePath, contents)
      expectedOfType(DirectoryChangeEvent.EventType.MODIFY)

      Files.delete(realFilePath)
      expectedOfType(DirectoryChangeEvent.EventType.DELETE)

      symlinkEventProbe.expectNoMessage(50.millis)
    } finally unobserve(id): Unit
  }
}

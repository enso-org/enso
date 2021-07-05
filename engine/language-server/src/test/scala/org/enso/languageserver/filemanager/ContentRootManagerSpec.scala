package org.enso.languageserver.filemanager

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestDuration, TestKit, TestProbe}
import org.apache.commons.lang3.SystemUtils
import org.enso.languageserver.data._
import org.enso.languageserver.filemanager.ContentRootManagerProtocol.{
  ContentRootsAddedNotification,
  SubscribeToNotifications
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.EitherValue
import org.scalatest.concurrent.Futures
import org.scalatest.concurrent.ScalaFutures.convertScalaFuture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{Inside, OptionValues}

import java.io.File
import java.nio.file.{Path => JPath}
import java.util.UUID
import scala.concurrent.duration.DurationInt

class ContentRootManagerSpec
    extends TestKit(ActorSystem("TestSystem"))
    with AnyWordSpecLike
    with Matchers
    with Futures
    with Inside
    with EitherValue
    with OptionValues {

  def makeContentRootManager(): (ContentRootManagerWrapper, ActorRef) = {
    val root = ContentRootWithFile(
      ContentRoot.Project(UUID.randomUUID()),
      new File("foobar").getCanonicalFile
    )
    val config = Config(
      root,
      FileManagerConfig(timeout = 3.seconds.dilated),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds.dilated),
      ProjectDirectoriesConfig.initialize(root.file)
    )
    val contentRootManagerActor =
      system.actorOf(ContentRootManagerActor.props(config))
    val contentRootManagerWrapper =
      new ContentRootManagerWrapper(config, contentRootManagerActor)
    (contentRootManagerWrapper, contentRootManagerActor)
  }

  "ContentRootManager" should {
    "provide filesystem roots" in {
      val (contentRootManager, _) = makeContentRootManager()
      val roots =
        contentRootManager.getContentRoots(system.dispatcher).futureValue
      val fsRoots =
        roots.collect {
          case ContentRootWithFile(ContentRoot.FileSystemRoot(_, path), _) =>
            path
        }

      if (SystemUtils.IS_OS_WINDOWS) {
        fsRoots should contain("C:\\")
      } else {
        fsRoots should contain("/")
        fsRoots should have size 1
      }
    }

    "allow to register new library roots and notify subscribers about them" in {
      val (wrapper, contentRootActor) = makeContentRootManager()

      val subscriberProbe = TestProbe("test-subscriber")
      subscriberProbe.send(contentRootActor, SubscribeToNotifications)

      inside(subscriberProbe.receiveOne(2.seconds.dilated)) {
        case ContentRootsAddedNotification(roots) =>
          val projectRoots = roots.collect {
            case ContentRootWithFile(ContentRoot.Project(_), _) =>
          }
          val fsRoots = roots.collect {
            case ContentRootWithFile(ContentRoot.Project(_), _) =>
          }
          projectRoots should have size 1
          fsRoots should not be empty
      }

      val rootPath = new File("foobar")

      system.eventStream.publish(
        Api.LibraryLoaded("Foo", "Bar", "local", rootPath)
      )

      inside(subscriberProbe.receiveOne(2.seconds.dilated)) {
        case ContentRootsAddedNotification(roots) =>
          roots should have length 1
          val root = roots.head
          inside(root) {
            case ContentRootWithFile(
                  ContentRoot.Library(_, namespace, name, version),
                  file
                ) =>
              file.getCanonicalFile shouldEqual rootPath.getCanonicalFile
              namespace shouldEqual "Foo"
              name shouldEqual "Bar"
              version shouldEqual "local"
          }
      }

      val roots = wrapper.getContentRoots(system.dispatcher).futureValue
      roots.exists {
        case ContentRootWithFile(
              ContentRoot.Library(_, "Foo", "Bar", "local"),
              _
            ) =>
          true
        case _ => false
      }
    }

    "return the root based on the id" in {
      val (rootManager, _) = makeContentRootManager()
      import system.dispatcher
      val roots         = rootManager.getContentRoots.futureValue
      val arbitraryRoot = roots.head
      val id            = arbitraryRoot.id

      rootManager
        .findContentRoot(id)
        .futureValue
        .rightValue shouldEqual arbitraryRoot
    }

    "relativize paths relative to the correct content root" in {
      val (rootManager, _) = makeContentRootManager()
      import system.dispatcher
      val roots = rootManager.getContentRoots.futureValue

      val projectRoots = roots.collect {
        case root @ ContentRootWithFile(ContentRoot.Project(_), _) => root
      }
      val fsRoots = roots.collect {
        case root @ ContentRootWithFile(ContentRoot.Project(_), _) => root
      }

      val projectRoot = projectRoots.head
      val someFsRoot  = fsRoots.head

      val projectPathRel = JPath.of("p1/foo")
      val projectPathAbsolute =
        projectRoot.file.toPath.resolve(projectPathRel).toFile
      val projectPathResolved =
        rootManager.findRelativePath(projectPathAbsolute).futureValue.value
      projectPathResolved.rootId shouldEqual projectRoot.id
      projectPathResolved.segments shouldEqual Vector("p1", "foo")

      val fsPathRel = JPath.of("fs/bar")
      val fsPathAbsolute =
        someFsRoot.file.toPath.resolve(fsPathRel).toFile
      val fsPathResolved =
        rootManager.findRelativePath(fsPathAbsolute).futureValue.value
      fsPathResolved.rootId shouldEqual someFsRoot.id
      fsPathResolved.segments shouldEqual Vector("fs", "bar")
    }
  }
}

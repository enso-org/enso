package org.enso.languageserver.filemanager

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestDuration, TestKit, TestProbe}
import org.apache.commons.lang3.SystemUtils
import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.languageserver.data._
import org.enso.languageserver.filemanager.ContentRootManagerProtocol.{
  ContentRootsAddedNotification,
  SubscribeToNotifications
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.EitherValue
import org.scalatest.{Inside, OptionValues}
import org.scalatest.concurrent.Futures
import org.scalatest.concurrent.ScalaFutures.convertScalaFuture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

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
      UUID.randomUUID(),
      ContentRootType.Project,
      "Project",
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
      val simplifiedRoots =
        roots.map(root => (root.name, root.`type`, root.file))

      if (SystemUtils.IS_OS_WINDOWS) {
        simplifiedRoots should contain(
          ("C:\\", ContentRootType.Root, new File("C:\\"))
        )
      } else {
        simplifiedRoots should contain(
          ("/", ContentRootType.Root, new File("/"))
        )
      }
    }

    "allow to register new library roots and notify subscribers about them" in {
      val (wrapper, contentRootActor) = makeContentRootManager()

      val subscriberProbe = TestProbe("test-subscriber")
      subscriberProbe.send(contentRootActor, SubscribeToNotifications)

      inside(subscriberProbe.receiveOne(2.seconds.dilated)) {
        case ContentRootsAddedNotification(roots) =>
          val projectRoot = roots.filter(_.`type` == ContentRootType.Project)
          projectRoot should have size 1
          roots.filter(_.`type` == ContentRootType.Root) should not be empty
      }

      val libraryName    = LibraryName("Foo", "Bar")
      val libraryVersion = LibraryVersion.Local
      val rootPath       = new File("foobar")
      val rootName       = "Foo.Bar:local"

      system.eventStream.publish(
        Api.LibraryLoaded(libraryName, libraryVersion, rootPath)
      )

      inside(subscriberProbe.receiveOne(2.seconds.dilated)) {
        case ContentRootsAddedNotification(roots) =>
          roots should have length 1
          val root = roots.head
          root.name shouldEqual rootName
          root.file.getCanonicalFile shouldEqual rootPath.getCanonicalFile
          root.`type` shouldEqual ContentRootType.Library
      }

      val roots = wrapper.getContentRoots(system.dispatcher).futureValue
      roots.map(r => r.name) should contain(rootName)
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

      val projectRoot = roots.filter(_.`type` == ContentRootType.Project).head
      val fsRoot      = roots.filter(_.`type` == ContentRootType.Root).head

      val projectPathRel = JPath.of("p1/foo")
      val projectPathAbsolute =
        projectRoot.file.toPath.resolve(projectPathRel).toFile
      val projectPathResolved =
        rootManager.findRelativePath(projectPathAbsolute).futureValue.value
      projectPathResolved.rootId shouldEqual projectRoot.id
      projectPathResolved.segments shouldEqual Vector("p1", "foo")

      val fsPathRel = JPath.of("fs/bar")
      val fsPathAbsolute =
        fsRoot.file.toPath.resolve(fsPathRel).toFile
      val fsPathResolved =
        rootManager.findRelativePath(fsPathAbsolute).futureValue.value
      fsPathResolved.rootId shouldEqual fsRoot.id
      fsPathResolved.segments shouldEqual Vector("fs", "bar")
    }
  }
}

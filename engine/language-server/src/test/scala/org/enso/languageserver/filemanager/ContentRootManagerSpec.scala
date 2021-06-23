package org.enso.languageserver.filemanager

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestDuration, TestKit}
import org.apache.commons.lang3.SystemUtils
import org.enso.languageserver.data._
import org.scalatest.concurrent.Futures
import org.scalatest.concurrent.ScalaFutures.convertScalaFuture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File
import java.util.UUID
import scala.concurrent.duration.DurationInt

class ContentRootManagerSpec
    extends TestKit(ActorSystem("TestSystem"))
    with AnyWordSpecLike
    with Matchers
    with Futures {

  def makeContentRootManager(): (ContentRootManagerWrapper, ActorRef) = {
    val root = ContentRootWithFile(
      UUID.randomUUID(),
      ContentRootType.Project,
      "Project",
      new File("foobar")
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

    "allow to register new library roots and notify subscribers about them" ignore {
      // TODO
    }
  }
}

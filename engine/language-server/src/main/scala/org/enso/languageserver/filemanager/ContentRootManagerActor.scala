package org.enso.languageserver.filemanager

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.ContentRootManagerActor.ContentRoots
import org.enso.languageserver.filemanager.ContentRootManagerProtocol._
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.io.File
import java.nio.file.{FileSystems, Path => JPath}
import java.util.UUID
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Try

class ContentRootManagerActor(config: Config)
    extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[Api.LibraryLoaded])
  }

  override def receive: Receive = mainStage(
    ContentRootManagerActor.initializeRoots(config),
    Set()
  )

  private def mainStage(
    contentRoots: ContentRoots,
    subscribers: Set[ActorRef]
  ): Receive = {
    val rootsMap = Map.from(contentRoots.toList.map(root => root.id -> root))

    {
      case Ping =>
        sender() ! Pong

      case GetContentRoots =>
        sender() ! GetContentRootsResult(contentRoots.toList)

      case SubscribeToNotifications =>
        sender() ! ContentRootsAddedNotification(contentRoots.toList)
        context.become(mainStage(contentRoots, subscribers + sender()))

      case Api.LibraryLoaded(libraryName, libraryVersion, rootPath) =>
        val rootName = s"$libraryName:$libraryVersion"

        val libraryRoot = ContentRootWithFile(
          id     = UUID.randomUUID(),
          `type` = ContentRootType.Library,
          name   = rootName,
          file   = rootPath.getCanonicalFile
        )

        subscribers.foreach { subscriber =>
          subscriber ! ContentRootsAddedNotification(List(libraryRoot))
        }

        context.become(
          mainStage(contentRoots.addLibraryRoot(libraryRoot), subscribers)
        )

      case FindContentRoot(id) =>
        sender() ! FindContentRootResult(rootsMap.get(id))

      case FindRelativePath(path) =>
        val relativized = contentRoots.findRelativePath(path)
        sender() ! FindRelativePathResult(relativized)
    }
  }
}

object ContentRootManagerActor {
  def props(config: Config): Props = Props(new ContentRootManagerActor(config))

  private case class ContentRoots(
    projectRoot: ContentRootWithFile,
    librariesRoots: List[ContentRootWithFile],
    homeRoots: List[ContentRootWithFile],
    filesystemRoots: List[ContentRootWithFile]
  ) {
    def addLibraryRoot(contentRoot: ContentRootWithFile): ContentRoots =
      copy(librariesRoots = contentRoot :: librariesRoots)

    /** Creates a list of available content roots.
      *
      * The order of the list is such that more specific directories are before
      * the more general ones, thus, when resolving paths, the more specific
      * roots will take precedence.
      */
    lazy val toList: List[ContentRootWithFile] =
      List(projectRoot) ++ librariesRoots ++ homeRoots ++ filesystemRoots

    def findRelativePath(path: File): Option[Path] =
      toList.flatMap { root =>
        if (path.toPath.startsWith(root.file.toPath)) {
          Some(Path(root.id, root.file.toPath.relativize(path.toPath)))
        } else {
          None
        }
      }.headOption
  }

  private def initializeRoots(config: Config): ContentRoots = {
    val fsRoots = FileSystems.getDefault.getRootDirectories.asScala.map {
      path =>
        val absolutePath = path.toAbsolutePath.normalize
        val name =
          Option(absolutePath.getRoot).map(_.toString).getOrElse("<root>")
        ContentRootWithFile(
          id     = UUID.randomUUID(),
          `type` = ContentRootType.Root,
          name   = name,
          file   = absolutePath.toFile
        )
    }

    val homeRoot = for {
      homeProp <- sys.props.get("user.home")
      homePath <- Try(JPath.of(homeProp)).toOption
    } yield ContentRootWithFile(
      id     = UUID.randomUUID(),
      `type` = ContentRootType.Home,
      name   = "Home",
      file   = homePath.toAbsolutePath.normalize.toFile
    )

    ContentRoots(
      projectRoot     = config.projectContentRoot,
      librariesRoots  = Nil,
      homeRoots       = homeRoot.toList,
      filesystemRoots = fsRoots.toList
    )
  }
}

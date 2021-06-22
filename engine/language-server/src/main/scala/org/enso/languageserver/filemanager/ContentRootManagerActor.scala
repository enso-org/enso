package org.enso.languageserver.filemanager

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.ContentRootManagerActor.ContentRoots
import org.enso.languageserver.filemanager.ContentRootManagerProtocol._
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging

import java.io.File

class ContentRootManagerActor(config: Config)
    extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = mainStage(
    ContentRootManagerActor.initializeRoots(config)
  )

  private def mainStage(contentRoots: ContentRoots): Receive = {
    val rootsMap = Map.from(contentRoots.toList.map(root => root.id -> root))

    {
      case Ping =>
        sender() ! Pong
      case GetContentRoots =>
        sender() ! GetContentRootsResult(contentRoots.toList)
      case AddLibraryRoot(_) =>
        ???
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
    // FIXME [RW] FS roots
    ContentRoots(config.projectContentRoot, Nil, Nil, Nil)
  }
}

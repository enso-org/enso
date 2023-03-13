package org.enso.languageserver.filemanager

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.ContentRootManagerActor.ContentRoots
import org.enso.languageserver.filemanager.ContentRootManagerProtocol._
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedPath
import org.enso.polyglot.runtime.Runtime.Api

import java.io.File
import java.nio.file.{FileSystems, Path => JPath}
import java.util.UUID
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Try

/** The Actor-based implementation of [[ContentRootManager]] that includes some
  * additional functionality.
  *
  * Aside from providing the desired request-response API, it allows other
  * Actors to subscribe to content root updates and will send them a
  * notification whenever a new content root is added.
  *
  * It watches the event stream for messages from the RuntimeConnector signaling
  * that new libraries are loaded, to register their content roots.
  */
class ContentRootManagerActor(config: Config)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[Api.LibraryLoaded])
  }

  /** @inheritdoc */
  override def receive: Receive = mainStage(
    ContentRootManagerActor.initializeRoots(config),
    Set()
  )

  /** The main and only state of the Actor.
    *
    * @param contentRoots the structure containing currently registered content
    *                     roots
    * @param subscribers a set of subscribers that should receive updates about
    *                    new content roots
    */
  private def mainStage(
    contentRoots: ContentRoots,
    subscribers: Set[ActorRef]
  ): Receive = {
    val rootsMap = Map.from(contentRoots.toList.map(root => root.id -> root))

    {
      case SubscribeToNotifications =>
        sender() ! ContentRootsAddedNotification(contentRoots.toList)
        context.become(mainStage(contentRoots, subscribers + sender()))

      case Api.LibraryLoaded(namespace, name, version, rootPath) =>
        val libraryRoot = ContentRootWithFile(
          ContentRoot.Library(
            id        = UUID.randomUUID(),
            namespace = namespace,
            name      = name,
            version   = version
          ),
          file = rootPath.getCanonicalFile
        )

        logger.trace(
          s"Library root for [$namespace.$name:$version] added at [{}].",
          MaskedPath(rootPath.toPath)
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

      case GetContentRoots =>
        sender() ! GetContentRootsResult(contentRoots.toList)

      case Ping =>
        sender() ! Pong
    }
  }
}

object ContentRootManagerActor {

  /** Creates a configuration object used to create a
    * [[ContentRootManagerActor]].
    */
  def props(config: Config): Props = Props(new ContentRootManagerActor(config))

  /** The structure that manages the registered content roots.
    *
    * The content roots are grouped based on their type, as the types also
    * define the specificity of the content root - the project and library
    * roots are most specific, then there are the home directories and at last,
    * the filesystem roots are most general, as every path will resolve as a
    * child of one of them.
    */
  private case class ContentRoots(
    projectRoot: ContentRootWithFile,
    librariesRoots: List[ContentRootWithFile],
    homeRoot: Option[ContentRootWithFile],
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
      List(projectRoot) ++ librariesRoots ++ homeRoot.toList ++ filesystemRoots

    /** Resolves the path as relative to one of the registered content roots.
      *
      * It tries to select the most specific content root associated with that
      * path, as described in the class documentation.
      */
    def findRelativePath(path: File): Option[Path] =
      toList.flatMap { root =>
        if (path.toPath.startsWith(root.file.toPath)) {
          Some(Path(root.id, root.file.toPath.relativize(path.toPath)))
        } else {
          None
        }
      }.headOption
  }

  /** Creates an initial content root configuration which consists of the main
    * project root, the home directory and filesystem roots.
    */
  private def initializeRoots(config: Config): ContentRoots = {
    val fsRoots = FileSystems.getDefault.getRootDirectories.asScala.map {
      path =>
        val absolutePath = path.toAbsolutePath.normalize
        ContentRootWithFile(
          ContentRoot.FileSystemRoot(
            id   = UUID.randomUUID(),
            path = absolutePath.toString
          ),
          file = absolutePath.toFile
        )
    }

    val homeRoot = for {
      homeProp <- sys.props.get("user.home")
      homePath <- Try(JPath.of(homeProp)).toOption
    } yield ContentRootWithFile(
      ContentRoot.Home(UUID.randomUUID()),
      file = homePath.toAbsolutePath.normalize.toFile
    )

    ContentRoots(
      projectRoot     = config.projectContentRoot,
      librariesRoots  = Nil,
      homeRoot        = homeRoot,
      filesystemRoots = fsRoots.toList
    )
  }
}

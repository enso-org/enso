package org.enso.filemanager

import akka.actor.typed.ActorRef
import io.methvin.watcher.DirectoryChangeEvent
import io.methvin.watcher.DirectoryWatcher
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.NotDirectoryException
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.UUID

import org.apache.commons.io.FileUtils
import org.enso.FileManager
import org.enso.filemanager.Detail.EventNotifier

import scala.reflect.ClassTag
import scala.util.Try

/** Container for types defined for File Manager API.
  *
  * Each File Manager operation is implemented as nested type with further
  * `Request` and `Response` subtypes.
  */
object API {
  import Request.Payload
  import Response.Success

  /** Base class for messages received by the [[FileManager]]. */
  type InputMessage = Request[_]

  /** Base class for messages that [[FileManager]] responds with. */
  type OutputMessage = Try[Response.Success]

  /**
    * Exception type that is raised on attempt to access to file outside the
    * project subtree.
    */
  final case class PathOutsideProjectException(
    projectRoot: Path,
    accessedPath: Path
  ) extends Exception(
        s"""Cannot access path $accessedPath because it does not belong to
           |the project under root directory $projectRoot""".stripMargin
          .replaceAll("\n", " ")
      )

  ////////////////////////
  //// RPC Definition ////
  ////////////////////////

  /** Request template, parametrised by the response type.
    */
  sealed case class Request[ResponseType <: Success: ClassTag](
    replyTo: ActorRef[Try[ResponseType]],
    contents: Payload[ResponseType]
  ) {

    def handle(fileManager: FileManager): Unit =
      fileManager.onMessageTyped(this)

    /** Throws a [[PathOutsideProjectException]] if request involves paths
      * outside the project subtree. */
    def validate(projectRoot: Path): Unit =
      contents.validate(projectRoot)
  }

  object Request {

    /** Base class for all the operation-specific contents of [[Request]]. */
    abstract class Payload[+ResponseType <: Success: ClassTag] {
      def touchedPaths:                     Seq[Path]
      def handle(fileManager: FileManager): ResponseType

      def validate(projectRoot: Path): Unit =
        touchedPaths.foreach(Detail.validatePath(_, projectRoot))
    }
  }

  object Response {
    sealed abstract class Success
  }

  //////////////////////////////
  //// Requests / Responses ////
  //////////////////////////////

  object CopyDirectory {
    case class Response() extends Success
    case class Request(from: Path, to: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(from, to)
      override def handle(fileManager: FileManager): Response = {
        FileUtils.copyDirectory(from.toFile, to.toFile)
        Response()
      }
    }
  }

  object CopyFile {
    case class Response() extends Success
    case class Request(from: Path, to: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] =
        Seq(from, to)
      override def handle(fileManager: FileManager): Response = {
        Files.copy(from, to)
        Response()
      }
    }
  }

  object DeleteDirectory {
    case class Response() extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        // Despite what commons-io documentation says, the exception is not
        // thrown when directory is missing, so we do it by hand.
        if (Files.notExists(path))
          throw new NoSuchFileException(path.toString)

        FileUtils.deleteDirectory(path.toFile)
        Response()
      }
    }
  }

  object DeleteFile {
    case class Response() extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        Files.delete(path)
        Response()
      }
    }
  }

  object Exists {
    case class Response(exists: Boolean) extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager) =
        Response(Files.exists(path))
    }
  }

  object List {
    case class Response(entries: Seq[Path]) extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        val str = Files.list(path)
        try {
          Response(str.toArray.toVector.map(_.asInstanceOf[Path]))
        } finally str.close()
      }
    }
  }

  object MoveDirectory {
    case class Response() extends Success
    case class Request(from: Path, to: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(from, to)
      override def handle(fileManager: FileManager): Response = {
        FileUtils.moveDirectory(from.toFile, to.toFile)
        Response()
      }
    }
  }

  object MoveFile {
    case class Response() extends Success
    case class Request(from: Path, to: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(from, to)
      override def handle(fileManager: FileManager): Response = {
        Files.move(from, to)
        Response()
      }
    }
  }

  object Read {
    case class Response(contents: Array[Byte]) extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        val contents = Files.readAllBytes(path)
        Response(contents)
      }
    }
  }

  object Status {
    case class Response(attributes: BasicFileAttributes) extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        val attributes =
          Files.readAttributes(path, classOf[BasicFileAttributes])
        Response(attributes)
      }
    }
  }

  object Touch {
    case class Response() extends Success
    case class Request(path: Path) extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        FileUtils.touch(path.toFile)
        Response()
      }
    }
  }

  /** Operations for managing filesystem watches, please see details.
    *
    * The watch will send [[FileSystemEvent]] to the observing agent when any
    * entry in the observed filesystem subtree is created, modified or deleted.
    * As this mechanism is built on top of [[java.nio.file.WatchService]] its
    * limitations and caveats apply. In particular:
    *  - events may come in different order;
    *  - events may not come at all if they undo each other (e.g. create and
    *  delete file in short time period, one modification may overshadow
    *  another);
    *  - duplicate notifications may be emitted for a single event;
    *  - deletion of child entries may not be observed if parent entry is;
    *  deleted;
    *  - all of the behaviors listed above are highly system dependent.
    *
    * Additionally:
    *  - watching is always recursive and must target a directory
    *  - the watched path must not be a symlink (though its parent path
    *  components are allowed to be symlinks)
    *  - if the observed path contains symlink, it will remain unresolved in the
    *  notification events (i.e. the event path prefix shall be the same as the
    *  observed subtree root).
    * */
  object Watch {

    object Create {
      case class Response(id: UUID) extends Success
      case class Request(
        observedDirPath: Path,
        observer: ActorRef[FileSystemEvent]
      ) extends Payload[Response] {
        override def touchedPaths: Seq[Path] = Seq(observedDirPath)
        override def handle(fileManager: FileManager): Response = {
          // Watching a symlink target works only on Windows, presumably thanks
          // to recursive watch being natively supported. We block this to keep
          // thinks uniform between platforms.
          if (Files.isSymbolicLink(observedDirPath))
            throw new NotDirectoryException(observedDirPath.toString)

          // Watching ordinary file throws an exception on Windows.
          // To unify behavior, we do this on all platforms.
          if (!Files.isDirectory(observedDirPath))
            throw new NotDirectoryException(observedDirPath.toString)

          val handler =
            EventNotifier(observedDirPath, observer, fileManager)
          val id = UUID.randomUUID()
          val watcher = DirectoryWatcher.builder
            .path(observedDirPath)
            .listener(handler.notify(_))
            .build()
          watcher.watchAsync()
          fileManager.watchers += (id -> watcher)
          Response(id)
        }
      }
    }

    object Remove {
      case class Response() extends Success
      case class Request(id: UUID) extends Payload[Response] {
        override def touchedPaths: Seq[Path] = Seq()
        override def handle(fileManager: FileManager): Response = {
          val watcher = fileManager.watchers(id)
          watcher.close()
          fileManager.watchers -= id
          Response()
        }
      }
    }
  }

  object Write {
    case class Response() extends Success
    case class Request(path: Path, contents: Array[Byte])
        extends Payload[Response] {
      override def touchedPaths: Seq[Path] = Seq(path)
      override def handle(fileManager: FileManager): Response = {
        Files.write(path, contents)
        Response()
      }
    }
  }

  case class FileSystemEvent(
    eventType: DirectoryChangeEvent.EventType,
    path: Path
  )
}

/** Implementation details, not expected to be relied on as path of API. */
object Detail {
  import API._

  def validatePath(validatedPath: Path, projectRoot: Path): Unit = {
    val normalized = validatedPath.toAbsolutePath.normalize()
    if (!normalized.startsWith(projectRoot))
      throw PathOutsideProjectException(projectRoot, validatedPath)
  }

  /** Helper class used for sending filesystem event notifications. */
  case class EventNotifier(
    observedPath: Path,
    observer: ActorRef[FileSystemEvent],
    fileManager: FileManager
  ) {

    val realObservedPath: Path           = observedPath.toRealPath()
    val observingUnresolvedPath: Boolean = observedPath != realObservedPath

    /** If the path prefix got resolved, restores the observed one.
      *
      * macOS generates events containing resolved path, i.e. with symlinks
      * resolved. We don't really want this, as we want to be completely
      * indifferent to symlink presence and still be able to easily compare
      * paths. Therefore if we are under symlink and generated event uses
      * real path, we replace it with path prefix that was observation
      * target.
      */
    def fixedPath(path: Path): Path = {
      val needsFixing = observingUnresolvedPath && path.startsWith(
          realObservedPath
        )
      needsFixing match {
        case true  => observedPath.resolve(realObservedPath.relativize(path))
        case false => path
      }
    }

    /** Notifies the observer about a given filesystem event. */
    def notify(event: DirectoryChangeEvent): Unit = {
      val message = FileSystemEvent(
        event.eventType,
        fixedPath(event.path)
      )
      if (message.path != observedPath) {
        val logText = s"Notifying $observer with $message"
        fileManager.context.log.debug(logText)
        observer ! message
      }
    }
  }
}

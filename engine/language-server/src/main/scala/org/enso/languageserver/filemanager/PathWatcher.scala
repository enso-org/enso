package org.enso.languageserver.filemanager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import org.enso.filewatcher.WatcherAdapter
import org.enso.languageserver.capability.CapabilityProtocol.{
  CapabilityAcquired,
  CapabilityAcquisitionFileSystemFailure,
  CapabilityForceReleased,
  CapabilityReleased
}
import org.enso.languageserver.data.{
  CapabilityRegistration,
  PathWatcherConfig,
  ReceivesTreeUpdates
}
import org.enso.languageserver.effect._
import org.enso.languageserver.event.JsonSessionTerminated
import org.enso.languageserver.util.UnhandledLogging
import zio._

import java.io.File

import scala.concurrent.Await
import scala.util.{Failure, Success}

/** Starts [[WatcherAdapter]], handles errors, converts and sends
  * events to the client.
  *
  * @param config configuration
  * @param contentRootManager the content root manager
  * @param fs file system
  * @param exec executor of file system effects
  */
final class PathWatcher(
  config: PathWatcherConfig,
  contentRootManager: ContentRootManager,
  fs: FileSystemApi[BlockingIO],
  exec: Exec[BlockingIO]
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import PathWatcherProtocol._
  import context.dispatcher

  private val restartCounter =
    new PathWatcher.RestartCounter(config.maxRestarts)
  private var fileWatcher: Option[WatcherAdapter] = None

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[JsonSessionTerminated]): Unit
  }
  override def postStop(): Unit = {
    stopWatcher(): Unit
  }

  override def receive: Receive = uninitializedStage

  private def uninitializedStage: Receive = { case WatchPath(path, clients) =>
    val pathToWatchResult = contentRootManager
      .findContentRoot(path.rootId)
      .map(result => result.map(root => path.toFile(root.file)))

    val result: BlockingIO[FileSystemFailure, Unit] =
      for {
        pathToWatch <- ZIO
          .fromFuture { _ => pathToWatchResult }
          .mapError { _ => ContentRootNotFound }
          .absolve
        _       <- validatePath(pathToWatch)
        watcher <- ZIO.fromEither(buildWatcher(pathToWatch))
        _       <- ZIO.fromEither(startWatcher(watcher))
      } yield ()

    exec
      .exec(result)
      .map {
        case Right(()) => CapabilityAcquired
        case Left(err) => CapabilityAcquisitionFileSystemFailure(err)
      }
      .pipeTo(sender())

    pathToWatchResult.onComplete {
      case Success(Right(root)) =>
        logger.info("Initialized for [{}]", path)
        context.become(initializedStage(root, path, clients))
      case Success(Left(err)) =>
        logger.error("Failed to resolve the path [{}]. {}", path, err)
        context.stop(self)
      case Failure(err) =>
        logger.error("Failed to resolve the path [{}]", path, err)
        context.stop(self)
    }
  }

  private def initializedStage(
    root: File,
    base: Path,
    clients: Set[ActorRef]
  ): Receive = {
    case WatchPath(_, newClients) =>
      sender() ! CapabilityAcquired
      context.become(initializedStage(root, base, clients ++ newClients))

    case UnwatchPath(client) =>
      sender() ! CapabilityReleased
      unregisterClient(root, base, clients - client)

    case JsonSessionTerminated(client)
        if clients.contains(client.rpcController) =>
      unregisterClient(root, base, clients - client.rpcController)

    case e: WatcherAdapter.WatcherEvent =>
      restartCounter.reset()
      val event = FileEvent.fromWatcherEvent(root, base, e)
      clients.foreach(_ ! FileEventResult(event))
      context.system.eventStream.publish(event)

    case WatcherAdapter.WatcherError(e) =>
      stopWatcher()
      restartCounter.inc()
      if (restartCounter.canRestart) {
        logger.error("Restart #{} on error.", restartCounter.count, e)
        context.system.scheduler.scheduleOnce(
          config.restartTimeout,
          self,
          WatchPath(base, clients)
        )
      } else {
        logger.error("Hit maximum number of restarts.", e)
        clients.foreach { client =>
          client ! CapabilityForceReleased(
            CapabilityRegistration(ReceivesTreeUpdates(base))
          )
        }
      }
      context.stop(self)
  }

  private def unregisterClient(
    root: File,
    base: Path,
    clients: Set[ActorRef]
  ): Unit = {
    if (clients.isEmpty) {
      context.stop(self)
    } else {
      context.become(initializedStage(root, base, clients))
    }
  }

  private def validatePath(path: File): BlockingIO[FileSystemFailure, Unit] =
    for {
      pathExists <- fs.exists(path)
      _          <- ZIO.when(!pathExists)(ZIO.fail(FileNotFound))
    } yield ()

  private def buildWatcher(
    path: File
  ): Either[FileSystemFailure, WatcherAdapter] =
    Either
      .catchNonFatal(WatcherAdapter.build(path.toPath, self ! _, self ! _))
      .leftMap(errorHandler)

  private def startWatcher(
    watcher: WatcherAdapter
  ): Either[FileSystemFailure, Unit] =
    Either
      .catchNonFatal {
        fileWatcher = Some(watcher)
        exec.exec_(ZIO.attempt(watcher.start()))
      }
      .leftMap(errorHandler)

  private def stopWatcher(): Either[FileSystemFailure, Unit] =
    Either
      .catchNonFatal {
        fileWatcher.foreach { watcher =>
          Await.ready(exec.exec(ZIO.attempt(watcher.stop())), config.timeout)
        }
      }
      .leftMap(errorHandler)

  private val errorHandler: Throwable => FileSystemFailure = { ex =>
    GenericFileSystemFailure(ex.getMessage)
  }
}

object PathWatcher {

  /** Counter for unsuccessful file watcher restarts.
    *
    * @param maxRestarts maximum restart attempts
    */
  final private class RestartCounter(maxRestarts: Int) {

    private var restartCount: Int = 0

    /** Return current restart count.
      */
    def count: Int =
      restartCount

    /** Increment restart count.
      */
    def inc(): Unit =
      restartCount += 1

    /** Reset restart count.
      */
    def reset(): Unit =
      restartCount = 0

    /** Return true if we hit the maximum number of restarts.
      */
    def canRestart: Boolean =
      restartCount < maxRestarts
  }

  /** Creates a configuration object used to create a [[PathWatcher]].
    *
    * @param config configuration
    * @param contentRootManager the content root manager
    * @param fs file system
    * @param exec executor of file system effects
    */
  def props(
    config: PathWatcherConfig,
    contentRootManager: ContentRootManager,
    fs: FileSystemApi[BlockingIO],
    exec: Exec[BlockingIO]
  ): Props =
    Props(new PathWatcher(config, contentRootManager, fs, exec))
}

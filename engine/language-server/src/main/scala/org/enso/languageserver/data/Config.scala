package org.enso.languageserver.data

import java.io.File
import java.util.UUID

import org.enso.languageserver.filemanager.{
  ContentRootNotFound,
  FileSystemFailure,
  Path
}

import scala.concurrent.duration._

/**
  * Configuration of the path watcher.
  *
  * @param timeout path watcher operations timeout
  * @param restartTimeout timeout before watcher is restarted on error
  * @param maxRestartsCount maximum number of unsuccessful restarts
  * before returning an error
  */
case class PathWatcherConfig(
  timeout: FiniteDuration,
  restartTimeout: FiniteDuration,
  maxRestarts: Int
)

object PathWatcherConfig {

  /**
    * Default path watcher config.
    */
  def apply(): PathWatcherConfig =
    PathWatcherConfig(
      timeout        = 5.seconds,
      restartTimeout = 5.seconds,
      maxRestarts    = 10
    )
}

/**
  * Configuration of the file manager.
  *
  * @param timeout IO operation timeout
  * @param parallelism number of processes working with the file system
  */
case class FileManagerConfig(timeout: FiniteDuration, parallelism: Int)

object FileManagerConfig {

  /**
    * Default file manager config.
    *
    * @param timeout IO operation timeout
    */
  def apply(timeout: FiniteDuration): FileManagerConfig =
    FileManagerConfig(
      timeout     = timeout,
      parallelism = Runtime.getRuntime().availableProcessors()
    )
}

/**
  * Configuration of the execution context.
  *
  * @param requestTimeout timeout of requests to the engine
  */
case class ExecutionContextConfig(requestTimeout: FiniteDuration)

object ExecutionContextConfig {

  /**
    * Default execution context config.
    */
  def apply(): ExecutionContextConfig =
    ExecutionContextConfig(
      requestTimeout = 5.seconds
    )
}

/**
  * The config of the running Language Server instance.
  *
  * @param contentRoots a mapping between content root id and absolute path to
  *                     the content root
  */
case class Config(
  contentRoots: Map[UUID, File],
  fileManager: FileManagerConfig,
  pathWatcher: PathWatcherConfig,
  executionContext: ExecutionContextConfig
) {

  def findContentRoot(rootId: UUID): Either[FileSystemFailure, File] =
    contentRoots
      .get(rootId)
      .toRight(ContentRootNotFound)

  def findRelativePath(path: File): Option[Path] =
    contentRoots.view.flatMap {
      case (id, root) =>
        if (path.toPath.startsWith(root.toPath)) {
          Some(Path(id, root.toPath.relativize(path.toPath)))
        } else {
          None
        }
    }.headOption

}

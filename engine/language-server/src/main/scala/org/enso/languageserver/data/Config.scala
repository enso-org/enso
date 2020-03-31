package org.enso.languageserver.data

import java.io.File
import java.util.UUID

import org.enso.languageserver.filemanager.{
  ContentRootNotFound,
  FileSystemFailure
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

case class FileManagerConfig(timeout: FiniteDuration, parallelism: Int)

object FileManagerConfig {

  def apply(timeout: FiniteDuration): FileManagerConfig =
    FileManagerConfig(
      timeout     = timeout,
      parallelism = Runtime.getRuntime().availableProcessors()
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
  pathWatcher: PathWatcherConfig
) {

  def findContentRoot(rootId: UUID): Either[FileSystemFailure, File] =
    contentRoots
      .get(rootId)
      .toRight(ContentRootNotFound)

}

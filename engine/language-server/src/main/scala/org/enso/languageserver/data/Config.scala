package org.enso.languageserver.data

import java.io.File
import java.nio.file.Files
import java.util.UUID

import org.enso.languageserver.filemanager.{
  ContentRootNotFound,
  FileSystemFailure,
  Path
}

import scala.concurrent.duration._

/** Configuration of the path watcher.
  *
  * @param timeout path watcher operations timeout
  * @param restartTimeout timeout before watcher is restarted on error
  * @param maxRestarts maximum number of unsuccessful restarts
  *                    before returning an error
  */
case class PathWatcherConfig(
  timeout: FiniteDuration,
  restartTimeout: FiniteDuration,
  maxRestarts: Int
)

object PathWatcherConfig {

  /** Default path watcher config.
    */
  def apply(): PathWatcherConfig =
    PathWatcherConfig(
      timeout        = 5.seconds,
      restartTimeout = 5.seconds,
      maxRestarts    = 10
    )
}

/** Configuration of the file manager.
  *
  * @param timeout IO operation timeout
  * @param parallelism number of processes working with the file system
  */
case class FileManagerConfig(timeout: FiniteDuration, parallelism: Int)

object FileManagerConfig {

  /** Default file manager config.
    *
    * @param timeout IO operation timeout
    */
  def apply(timeout: FiniteDuration): FileManagerConfig =
    FileManagerConfig(
      timeout     = timeout,
      parallelism = Runtime.getRuntime.availableProcessors()
    )
}

/** Configuration of the execution context.
  *
  * @param requestTimeout timeout of requests to the engine
  */
case class ExecutionContextConfig(requestTimeout: FiniteDuration)

object ExecutionContextConfig {

  /** Default execution context config.
    */
  def apply(): ExecutionContextConfig =
    ExecutionContextConfig(
      requestTimeout = 5.seconds
    )
}

/** Configuration of directories for storing internal files.
  *
  * @param root the root directory path
  */
case class DirectoriesConfig(root: File) {

  /** The data directory path. */
  val dataDirectory: File =
    new File(root, DirectoriesConfig.DataDirectory)

  /** The suggestions database file path. */
  val suggestionsDatabaseFile: File =
    new File(dataDirectory, DirectoriesConfig.SuggestionsDatabaseFile)

  /** Create data directories if not exist. */
  private def createDirectories(): Unit =
    Files.createDirectories(dataDirectory.toPath)
}

object DirectoriesConfig {

  val DataDirectory: String           = ".enso"
  val SuggestionsDatabaseFile: String = "suggestions.db"

  /** Create default data directory config, creating directories if not exist.
    *
    * @param root the root directory path
    * @return data directory config
    */
  def initialize(root: String): DirectoriesConfig =
    initialize(new File(root))

  /** Create default data directory config, creating directories if not exist.
    *
    * @param root the root directory path
    * @return data directory config
    */
  def initialize(root: File): DirectoriesConfig = {
    val config = new DirectoriesConfig(root)
    config.createDirectories()
    config
  }
}

/** The config of the running Language Server instance.
  *
  * @param contentRoots a mapping between content root id and absolute path to
  * the content root
  * @param fileManager the file manager config
  * @param pathWatcher the path watcher config
  * @param executionContext the executionContext config
  * @param directories the configuration of internal directories
  */
case class Config(
  contentRoots: Map[UUID, File],
  fileManager: FileManagerConfig,
  pathWatcher: PathWatcherConfig,
  executionContext: ExecutionContextConfig,
  directories: DirectoriesConfig
) {

  def findContentRoot(rootId: UUID): Either[FileSystemFailure, File] =
    contentRoots
      .get(rootId)
      .toRight(ContentRootNotFound)

  def findRelativePath(path: File): Option[Path] =
    contentRoots.view.flatMap { case (id, root) =>
      if (path.toPath.startsWith(root.toPath)) {
        Some(Path(id, root.toPath.relativize(path.toPath)))
      } else {
        None
      }
    }.headOption

}

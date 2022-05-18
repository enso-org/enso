package org.enso.languageserver.data

import org.enso.languageserver.boot.ProfilingConfig
import org.enso.languageserver.filemanager.ContentRootWithFile
import org.enso.logger.masking.{MaskingUtils, ToLogString}

import java.io.File
import java.nio.file.Files

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
case class ProjectDirectoriesConfig(root: File) extends ToLogString {

  /** The data directory path. */
  val dataDirectory: File =
    new File(root, ProjectDirectoriesConfig.DataDirectory)

  /** The suggestions database file path. */
  val suggestionsDatabaseFile: File =
    new File(dataDirectory, ProjectDirectoriesConfig.SuggestionsDatabaseFile)

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String = {
    val rootString =
      if (shouldMask) MaskingUtils.toMaskedPath(root.toPath)
      else root.toString
    s"DirectoriesConfig($rootString)"
  }

  /** Create data directories if not exist. */
  def createDirectories(): Unit =
    Files.createDirectories(dataDirectory.toPath)
}

object ProjectDirectoriesConfig {

  val DataDirectory: String           = ".enso"
  val SuggestionsDatabaseFile: String = "suggestions.db"

  def apply(root: String): ProjectDirectoriesConfig =
    new ProjectDirectoriesConfig(new File(root))

  /** Create default data directory config, creating directories if not exist.
    *
    * @param root the root directory path
    * @return data directory config
    */
  def initialize(root: File): ProjectDirectoriesConfig = {
    val config = new ProjectDirectoriesConfig(root)
    config.createDirectories()
    config
  }
}

/** The config of the running Language Server instance.
  *
  * @param projectContentRoot project's main content root
  * @param fileManager the file manager config
  * @param pathWatcher the path watcher config
  * @param executionContext the executionContext config
  * @param directories the configuration of internal directories
  * @param profiling the profiling configuration
  */
case class Config(
  projectContentRoot: ContentRootWithFile,
  fileManager: FileManagerConfig,
  pathWatcher: PathWatcherConfig,
  executionContext: ExecutionContextConfig,
  directories: ProjectDirectoriesConfig,
  profiling: ProfilingConfig
) extends ToLogString {

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String = {
    val maskedRoot =
      if (shouldMask) {
        MaskingUtils.toMaskedPath(projectContentRoot.file.toPath)
      } else {
        projectContentRoot
      }
    s"Config(" +
    s"projectContentRoot=$maskedRoot, " +
    s"fileManager=$fileManager, " +
    s"pathWatcher=$pathWatcher, " +
    s"executionContext=$executionContext, " +
    s"directories=${directories.toLogString(shouldMask)}" +
    s")"
  }
}
object Config {
  def ensoPackageConfigName: String = "package.yaml"
}

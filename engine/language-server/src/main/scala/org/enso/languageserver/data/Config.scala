package org.enso.languageserver.data

import org.enso.languageserver.boot.{ProfilingConfig, StartupConfig}
import org.enso.languageserver.filemanager.ContentRootWithFile
import org.enso.logger.masking.{MaskedPath, ToLogString}

import java.io.File
import java.nio.file.{Files, Path}

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

/** Configuration of the VCS manager.
  *
  * @param initTimeout vcs init operation timeout
  * @param timeout default vcs operation timeout
  * @param asyncInit flag indicating that vcs initialization should be non-blocking
  */
case class VcsManagerConfig(
  initTimeout: FiniteDuration,
  timeout: FiniteDuration,
  asyncInit: Boolean
) {
  val dataDirectory: Path =
    Path.of(ProjectDirectoriesConfig.DataDirectory)
}

object VcsManagerConfig {
  def apply(asyncInit: Boolean = true): VcsManagerConfig =
    VcsManagerConfig(initTimeout = 5.seconds, 5.seconds, asyncInit)
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
    val rootString = MaskedPath(root.toPath).toLogString(shouldMask)
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
  * @param startup the startup configuration
  */
case class Config(
  projectContentRoot: ContentRootWithFile,
  fileManager: FileManagerConfig,
  vcsManager: VcsManagerConfig,
  pathWatcher: PathWatcherConfig,
  executionContext: ExecutionContextConfig,
  directories: ProjectDirectoriesConfig,
  profiling: ProfilingConfig,
  startup: StartupConfig
) extends ToLogString {

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String =
    s"Config(" +
    s"projectContentRoot=${projectContentRoot.toLogString(shouldMask)}, " +
    s"fileManager=$fileManager, " +
    s"vcsManager=$vcsManager, " +
    s"pathWatcher=$pathWatcher, " +
    s"executionContext=$executionContext, " +
    s"directories=${directories.toLogString(shouldMask)}" +
    ")"

}
object Config {
  def ensoPackageConfigName: String = "package.yaml"
}

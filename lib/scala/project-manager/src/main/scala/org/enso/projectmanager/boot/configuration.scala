package org.enso.projectmanager.boot

import org.enso.loggingservice.LogLevel

import java.io.File
import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

object configuration {

  /** The options supplied (e.g. with the command line options when starting the
    *  main project manager process.
    *
    *  @param logLevel the logging level
    *  @param profilingEventsLogPath the path to the runtime events log file
    *  @param profilingPath the path to the profiling out file
    *  @param profilingTime the time limiting the profiling duration
    */
  case class MainProcessConfig(
    logLevel: LogLevel,
    profilingEventsLogPath: Option[Path],
    profilingPath: Option[Path],
    profilingTime: Option[FiniteDuration]
  )

  /** A configuration object for properties of the Project Manager.
    *
    * @param server a JSON RPC server configuration
    */
  case class ProjectManagerConfig(
    server: ServerConfig,
    storage: StorageConfig,
    timeout: TimeoutConfig,
    network: NetworkConfig,
    bootloader: BootloaderConfig,
    supervision: SupervisionConfig
  )

  /** A configuration object for properties of the JSON RPC server.
    *
    * @param host an address that the server listen on
    * @param port a port that the server listen on
    */
  case class ServerConfig(host: String, port: Int)

  /** A configuration object for properties of project storage.
    *
    * @param projectsRoot a project root
    * @param userProjectsPath a user project root
    * @param projectMetadataDirectory a directory name containing project
    *                                 metadata
    * @param projectMetadataFileName a name of project metadata file
    */
  case class StorageConfig(
    projectsRoot: File,
    userProjectsPath: File,
    projectMetadataDirectory: String,
    projectMetadataFileName: String
  )

  /** A configuration object for timeout properties.
    *
    * @param ioTimeout a timeout for IO operations
    * @param requestTimeout a timeout for JSON RPC request timeout
    */
  case class TimeoutConfig(
    ioTimeout: FiniteDuration,
    requestTimeout: FiniteDuration,
    bootTimeout: FiniteDuration,
    shutdownTimeout: FiniteDuration,
    socketCloseTimeout: FiniteDuration
  )

  /** A configuration object for networking.
    *
    * @param interface an interface to listen to
    * @param minPort min port for the LS
    * @param maxPort max port for the LS
    */
  case class NetworkConfig(interface: String, minPort: Int, maxPort: Int)

  /** A configuration object for bootloader properties.
    *
    * @param numberOfRetries how many times a bootloader should try to boot the LS
    * @param delayBetweenRetry delays between retries
    */
  case class BootloaderConfig(
    numberOfRetries: Int,
    delayBetweenRetry: FiniteDuration
  )

  /** A configuration object for supervisor properties.
    *
    * @param initialDelay a time that the supervisor wait before starts
    *                     monitoring
    * @param heartbeatInterval an interval between heartbeat sessions
    * @param heartbeatTimeout a timeout for pong reply
    * @param numberOfRestarts a maximum number of restarts
    * @param delayBetweenRestarts a delay between server restarts
    */
  case class SupervisionConfig(
    initialDelay: FiniteDuration,
    heartbeatInterval: FiniteDuration,
    heartbeatTimeout: FiniteDuration,
    numberOfRestarts: Int,
    delayBetweenRestarts: FiniteDuration
  )
}

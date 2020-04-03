package org.enso.projectmanager.boot

import java.io.File

import scala.concurrent.duration.FiniteDuration

object configuration {

  /**
    * A configuration object for properties of the Project Manager.
    *
    * @param server a JSON RPC server configuration
    */
  case class ProjectManagerConfig(
    server: ServerConfig,
    storage: StorageConfig,
    timeout: TimeoutConfig,
    network: NetworkConfig,
    bootloader: BootloaderConfig
  )

  /**
    * A configuration object for properties of the JSON RPC server.
    *
    * @param host an address that the server listen on
    * @param port a port that the server listen on
    */
  case class ServerConfig(host: String, port: Int)

  /**
    * A configuration object for properties of project storage.
    *
    * @param projectsRoot a project root
    * @param projectIndexPath a path to the index
    * @param userProjectsPath a user project root
    */
  case class StorageConfig(
    projectsRoot: File,
    projectIndexPath: File,
    userProjectsPath: File
  )

  /**
    * A configuration object for timeout properties.
    *
    * @param ioTimeout a timeout for IO operations
    * @param requestTimeout a timeout for JSON RPC request timeout
    */
  case class TimeoutConfig(
    ioTimeout: FiniteDuration,
    requestTimeout: FiniteDuration,
    bootTimeout: FiniteDuration
  )

  /**
    * A configuration object for networking.
    *
    * @param interface an interface to listen to
    * @param minPort min port for the LS
    * @param maxPort max port for the LS
    */
  case class NetworkConfig(interface: String, minPort: Int, maxPort: Int)

  /**
    * A configuration object for bootloader properties.
    *
    * @param numberOfRetries how many times a bootloader should try to boot the LS
    * @param delayBetweenRetry delays between retries
    */
  case class BootloaderConfig(
    numberOfRetries: Int,
    delayBetweenRetry: FiniteDuration
  )

}

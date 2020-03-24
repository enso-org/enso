package org.enso.projectmanager.main

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
    timeout: TimeoutConfig
  )

  /**
    * A configuration object for properties of the JSON RPC server.
    *
    * @param host an address that the server listen on
    * @param port a port that the server listen on
    */
  case class ServerConfig(host: String, port: Int)

  case class StorageConfig(
    projectsRoot: File,
    projectMetadataPath: File,
    userProjectsPath: File
  )

  /**
    * A configuration object for timeout proeperties.
    *
    * @param ioTimeout a timeout for IO operations
    * @param requestTimeout a timeout for JSON RPC request timeout
    */
  case class TimeoutConfig(
    ioTimeout: FiniteDuration,
    requestTimeout: FiniteDuration
  )

}

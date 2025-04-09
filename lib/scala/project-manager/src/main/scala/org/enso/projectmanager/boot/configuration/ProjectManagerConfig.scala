package org.enso.projectmanager.boot.configuration

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

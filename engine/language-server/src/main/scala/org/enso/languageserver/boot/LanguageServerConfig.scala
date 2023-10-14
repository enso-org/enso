package org.enso.languageserver.boot

import java.util.UUID

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**  The config of the running Language Server instance.
  *
  * @param interface a interface that the server listen to
  * @param rpcPort a rpc port that the server listen to
  * @param secureRpcPort an optional secure rpc port that the server listen to
  * @param dataPort a data port that the server listen to
  * @param secureDataPort an optional secure data port that the server listen to
  * @param contentRootUuid an id of content root
  * @param contentRootPath a path to the content root
  * @param profilingConfig an application profiling configuration
  * @param startupConfig a startup configuration
  */
case class LanguageServerConfig(
  interface: String,
  rpcPort: Int,
  secureRpcPort: Option[Int],
  dataPort: Int,
  secureDataPort: Option[Int],
  contentRootUuid: UUID,
  contentRootPath: String,
  profilingConfig: ProfilingConfig,
  startupConfig: StartupConfig,
  name: String                                      = "language-server",
  computeExecutionContext: ExecutionContextExecutor = ExecutionContext.global
)

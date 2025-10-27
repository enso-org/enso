package org.enso.languageserver.boot

import java.util.UUID

import org.enso.runner.common.ProfilingConfig
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
  * @param projectId an id of project
  * @param profilingConfig an application profiling configuration
  * @param startupConfig a startup configuration
  * @param logMasking a flag indicating if the log masking is enabled
  */
case class LanguageServerConfig(
  interface: String,
  rpcPort: Int,
  secureRpcPort: Option[Int],
  dataPort: Int,
  secureDataPort: Option[Int],
  contentRootUuid: UUID,
  contentRootPath: String,
  projectId: UUID,
  profilingConfig: ProfilingConfig,
  startupConfig: StartupConfig,
  logMasking: Boolean,
  name: String                                      = "language-server",
  computeExecutionContext: ExecutionContextExecutor = ExecutionContext.global
)

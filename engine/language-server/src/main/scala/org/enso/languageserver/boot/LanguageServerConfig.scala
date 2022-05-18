package org.enso.languageserver.boot

import java.nio.file.Path
import java.util.UUID

import scala.concurrent.ExecutionContext

/**  The config of the running Language Server instance.
  *
  * @param interface a interface that the server listen to
  * @param rpcPort a rpc port that the server listen to
  * @param dataPort a data port that the server listen to
  * @param contentRootUuid an id of content root
  * @param contentRootPath a path to the content root
  * @param profilingPath a path to the profiling output file
  */
case class LanguageServerConfig(
  interface: String,
  rpcPort: Int,
  dataPort: Int,
  contentRootUuid: UUID,
  contentRootPath: String,
  profilingPath: Option[Path],
  name: String                              = "language-server",
  computeExecutionContext: ExecutionContext = ExecutionContext.global
)

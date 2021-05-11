package org.enso.languageserver.boot

import java.nio.file.Path
import java.util.UUID

import org.enso.logger.masking.MaskingUtils.toMaskedPath
import org.enso.logger.masking.ToMaskedString

import scala.concurrent.ExecutionContext

/**  The config of the running Language Server instance.
  *
  * @param interface a interface that the server listen to
  * @param rpcPort a rpc port that the server listen to
  * @param dataPort a data port that the server listen to
  * @param contentRootUuid an id of content root
  * @param contentRootPath a path to the content root
  */
case class LanguageServerConfig(
  interface: String,
  rpcPort: Int,
  dataPort: Int,
  contentRootUuid: UUID,
  contentRootPath: String,
  name: String                              = "language-server",
  computeExecutionContext: ExecutionContext = ExecutionContext.global
) extends ToMaskedString {

  /** @inheritdoc */
  override def toString: String =
    s"""LanguageServerConfig(
       |  interface       = $interface
       |  rpcPort         = $rpcPort
       |  dataPort        = $dataPort
       |  contentRootUuid = $contentRootUuid
       |  contentRootPath = $contentRootPath
       |  name            = $name
       |  computeExecutionContext = $computeExecutionContext
       |)""".stripMargin

  /** @inheritdoc */
  override def toMaskedString: String =
    s"""LanguageServerConfig(
       |  interface       = $interface
       |  rpcPort         = $rpcPort
       |  dataPort        = $dataPort
       |  contentRootUuid = $contentRootUuid
       |  contentRootPath = ${toMaskedPath(Path.of(contentRootPath))}
       |  name            = $name
       |  computeExecutionContext = $computeExecutionContext
       |  interface = $interface
       |)""".stripMargin
}

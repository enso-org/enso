package org.enso.languageserver.boot

import java.nio.file.Path
import java.util.UUID

import org.enso.logger.masking.MaskingUtils.toMaskedPath
import org.enso.logger.masking.ToLogString

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
) extends ToLogString {

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String = {
    val contentRootString =
      if (shouldMask) {
        toMaskedPath(Path.of(contentRootPath))
      } else {
        contentRootPath
      }
    s"LanguageServerConfig(" +
    s"interface=$interface, " +
    s"rpcPort=$rpcPort, " +
    s"dataPort=$dataPort, " +
    s"contentRootUuid=$contentRootUuid, " +
    s"contentRootPath=$contentRootString, " +
    s"name=$name, " +
    s"computeExecutionContext=$computeExecutionContext" +
    s")"
  }
}

package org.enso.runtimeversionmanager.runner

import java.util.UUID

/** Options that are passed to the language server.
  *
  * @param rootId an id of content root
  * @param projectId an id of the project
  * @param interface a interface that the server listen to
  * @param rpcPort an RPC port that the server listen to
  * @param secureRpcPort an option secure RPC port that the server listen to
  * @param dataPort a data port that the server listen to
  * @param secureDataPort an optional secure data port that the server listen to
  * @param jvmModeEnabled enables JVM mode if true
  */
case class LanguageServerOptions(
  rootId: UUID,
  projectId: UUID,
  interface: String,
  rpcPort: Int,
  secureRpcPort: Option[Int],
  dataPort: Int,
  secureDataPort: Option[Int],
  jvmModeEnabled: Boolean
)

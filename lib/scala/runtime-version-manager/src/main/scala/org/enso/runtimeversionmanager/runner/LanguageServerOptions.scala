package org.enso.runtimeversionmanager.runner

import java.util.UUID

/** Options that are passed to the language server.
  *
  * @param rootId an id of content root
  * @param interface a interface that the server listen to
  * @param rpcPort an RPC port that the server listen to
  * @param dataPort a data port that the server listen to
  */
case class LanguageServerOptions(
  rootId: UUID,
  interface: String,
  rpcPort: Int,
  dataPort: Int
)

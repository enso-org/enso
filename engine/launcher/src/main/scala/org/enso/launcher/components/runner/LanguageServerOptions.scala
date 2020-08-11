package org.enso.launcher.components.runner

import java.nio.file.Path
import java.util.UUID

/**
  * Options that are passed to the language server.
  *
  * @param rootId an id of content root
  * @param path a path to the content root
  * @param interface a interface that the server listen to
  * @param rpcPort an RPC port that the server listen to
  * @param dataPort a data port that the server listen to
  */
case class LanguageServerOptions(
  rootId: UUID,
  path: Path,
  interface: String,
  rpcPort: Int,
  dataPort: Int
)

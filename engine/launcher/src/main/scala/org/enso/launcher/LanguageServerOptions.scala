package org.enso.launcher

import java.nio.file.Path
import java.util.UUID

case class LanguageServerOptions(
  rootId: UUID,
  path: Path,
  interface: String,
  rpcPort: Int,
  dataPort: Int
)

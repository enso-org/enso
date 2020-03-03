package org.enso.runner

import java.util.UUID

case class LanguageServerConfig(
  interface: String,
  port: Int,
  contentRootUuid: UUID,
  contentRootPath: String
)

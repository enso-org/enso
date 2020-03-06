package org.enso.languageserver

import java.util.UUID

/**
  *  The config of the running Language Server instance.
  *
  * @param interface a interface that the server listen to
  * @param port a port that the server listen to
  * @param contentRootUuid an id of content root
  * @param contentRootPath a path to the content root
  */
case class LanguageServerConfig(
  interface: String,
  port: Int,
  contentRootUuid: UUID,
  contentRootPath: String
)

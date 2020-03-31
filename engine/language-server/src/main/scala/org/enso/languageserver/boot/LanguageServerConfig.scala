package org.enso.languageserver.boot

import java.util.UUID

import scala.concurrent.ExecutionContext

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
  contentRootPath: String,
  name: String                              = "language-server",
  computeExecutionContext: ExecutionContext = ExecutionContext.global
)

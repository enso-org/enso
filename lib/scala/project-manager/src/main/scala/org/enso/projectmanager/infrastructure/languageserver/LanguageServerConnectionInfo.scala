package org.enso.projectmanager.infrastructure.languageserver

/** Describes how to connect to a Language Server instance by providing its
  * interface and selected ports.
  */
case class LanguageServerConnectionInfo(
  interface: String,
  rpcPort: Int,
  secureRpcPort: Option[Int],
  dataPort: Int,
  secureDataPort: Option[Int]
)

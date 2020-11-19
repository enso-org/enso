package org.enso.projectmanager.infrastructure.languageserver

case class LanguageServerConnectionInfo(
  interface: String,
  rpcPort: Int,
  dataPort: Int
)

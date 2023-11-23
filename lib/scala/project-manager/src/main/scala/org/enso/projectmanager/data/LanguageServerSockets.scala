package org.enso.projectmanager.data

/** Sockets that a language server listens on.
  *
  * @param jsonSocket a socket used for JSON-RPC protocol
  * @param secureJsonSocket a secure socket used for JSON-RPC protocol
  * @param binarySocket a socket used for the binary protocol
  * @param secureBinarySocket a secure socket used for the binary protocol
  */
case class LanguageServerSockets(
  jsonSocket: Socket,
  secureJsonSocket: Option[Socket],
  binarySocket: Socket,
  secureBinarySocket: Option[Socket]
)

package org.enso.projectmanager.data

/**
  * Sockets that a language server listens on.
  *
  * @param rpcSocket a socket used for RPC protocol
  * @param dataSocket a socket used fot data protocol
  */
case class LanguageServerSockets(rpcSocket: Socket, dataSocket: Socket)

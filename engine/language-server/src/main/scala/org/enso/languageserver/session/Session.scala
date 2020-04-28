package org.enso.languageserver.session

import org.enso.languageserver.data.ClientId

/**
  * An object representing a client connected to the language server via both
  * rpc and data protocol.
  *
  * @param clientId the internal id of this client
  * @param maybeRpcSession a session for rpc protocol
  * @param maybeDataSession a session for data protocol
  */
case class Session(
  clientId: ClientId,
  maybeRpcSession: Option[RpcSession],
  maybeDataSession: Option[DataSession]
) {

  def attachRpcSession(rpcSession: RpcSession): Session =
    this.copy(maybeRpcSession = Some(rpcSession))

  def attachDataSession(dataSession: DataSession): Session =
    this.copy(maybeDataSession = Some(dataSession))

  def detachRpcSession(): Session = this.copy(maybeRpcSession = None)

  def detachDataSession(): Session = this.copy(maybeDataSession = None)

  def isSessionTerminated: Boolean =
    maybeRpcSession.isEmpty && maybeDataSession.isEmpty

}

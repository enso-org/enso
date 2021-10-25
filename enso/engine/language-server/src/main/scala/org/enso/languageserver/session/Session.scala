package org.enso.languageserver.session

import org.enso.languageserver.data.ClientId

/** An object representing a client connected to the language server via both
  * rpc and data protocol.
  *
  * @param clientId the internal id of this client
  * @param maybeJsonSession a session for rpc protocol
  * @param maybeBinarySession a session for data protocol
  */
case class Session(
  clientId: ClientId,
  maybeJsonSession: Option[JsonSession],
  maybeBinarySession: Option[BinarySession]
) {

  def attachJsonSession(rpcSession: JsonSession): Session =
    this.copy(maybeJsonSession = Some(rpcSession))

  def attachBinarySession(dataSession: BinarySession): Session =
    this.copy(maybeBinarySession = Some(dataSession))

  def detachJsonSession(): Session = this.copy(maybeJsonSession = None)

  def detachBinarySession(): Session = this.copy(maybeBinarySession = None)

  def isSessionTerminated: Boolean =
    maybeJsonSession.isEmpty && maybeBinarySession.isEmpty

}

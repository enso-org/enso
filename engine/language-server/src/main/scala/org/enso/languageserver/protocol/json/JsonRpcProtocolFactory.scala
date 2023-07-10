package org.enso.languageserver.protocol.json

import org.enso.jsonrpc
import org.enso.jsonrpc.{Errors, Protocol, ProtocolFactory}
import org.enso.languageserver.session.SessionApi.SessionNotInitialisedError

/** Factory creating JSON-RPC protocol. */
final class JsonRpcProtocolFactory extends ProtocolFactory {

  private var _protocol: Protocol = _

  /** @inheritdoc */
  def getProtocol(): Protocol = {
    if (_protocol == null) {
      _protocol = JsonRpc.initProtocol
    }
    _protocol
  }

  /** @inheritdoc */
  override def init(): Unit = {
    if (_protocol == null) {
      _protocol = JsonRpc.initProtocol
    }
    _protocol = JsonRpc.fullProtocol(_protocol)
  }

  /** Error returned when a requested method is not recognized */
  override def onMissingMethod(): jsonrpc.Error = {
    if (_protocol != null && _protocol.initialized) Errors.MethodNotFound
    else SessionNotInitialisedError
  }
}

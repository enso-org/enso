package org.enso.languageserver.protocol.json

import org.enso.jsonrpc.{Protocol, ProtocolFactory}

/** Factory creating JSON-RPC protocol. */
final class JsonRpcProtocolFactory extends ProtocolFactory {

  private[this] var _protocol: Protocol = _

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
}

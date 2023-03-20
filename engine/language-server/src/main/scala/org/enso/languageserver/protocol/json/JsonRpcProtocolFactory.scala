package org.enso.languageserver.protocol.json

import org.enso.jsonrpc.{Protocol, ProtocolFactory}

/** Factory creating JSON-RPC protocol. */
final class JsonRpcProtocolFactory extends ProtocolFactory {

  /** @inheritdoc */
  def getProtocol: Protocol =
    JsonRpc.protocol

  /** @inheritdoc */
  override def init(): Unit = {
    val _ = JsonRpc.protocol
  }
}

package org.enso.projectmanager.protocol

import org.enso.jsonrpc.{Protocol, ProtocolFactory}

/** Factory creating JSON-RPC protocol. */
final class JsonRpcProtocolFactory extends ProtocolFactory {

  /** @inheritdoc */
  override def getProtocol: Protocol =
    JsonRpc.protocol

  /** @inheritdoc */
  override def init(): Unit = {
    val _ = JsonRpc.protocol
  }
}

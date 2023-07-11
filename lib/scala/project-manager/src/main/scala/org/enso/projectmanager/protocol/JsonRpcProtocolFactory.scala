package org.enso.projectmanager.protocol

import org.enso.jsonrpc
import org.enso.jsonrpc.{Errors, Protocol, ProtocolFactory}

/** Factory creating JSON-RPC protocol. */
final class JsonRpcProtocolFactory extends ProtocolFactory {

  /** @inheritdoc */
  override def getProtocol(): Protocol =
    JsonRpc.protocol

  /** @inheritdoc */
  override def init(): Unit = {
    val _ = JsonRpc.protocol
  }

  /** Error returned when a requested method is not recognized */
  override def onMissingMethod(): jsonrpc.Error = Errors.MethodNotFound
}

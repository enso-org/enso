package org.enso.jsonrpc

/** Factory that creates [[Protocol]]. */
trait ProtocolFactory {

  /** @return the [[Protocol]] instance. */
  def getProtocol: Protocol

  /** Initialize the protocol. */
  def init(): Unit
}

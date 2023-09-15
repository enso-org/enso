package org.enso.jsonrpc

/** Factory that creates [[Protocol]]. */
trait ProtocolFactory {

  /** Returns the [[Protocol]] instance.
    * If the factory has not been properly initialized yet, returns only a minimal set of messages
    * supported during the initialization period. Returns a full set of supported messages in the
    * post-initialization stage.
    *
    * @return the [[Protocol]] instance.
    */
  def getProtocol(): Protocol

  /** Initialize the protocol with the full set of supported messages. */
  def init(): Unit

  /** Error returned when a requested method is not recognized */
  def onMissingMethod(): Error
}

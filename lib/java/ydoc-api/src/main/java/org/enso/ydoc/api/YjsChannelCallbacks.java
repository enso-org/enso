package org.enso.ydoc.api;

/**
 * Callback interface for receiving newly established {@link YjsChannel} connections.
 *
 * <p>Implementations handle the lifecycle of channels between the Language Server and Ydoc server.
 * The Ydoc server invokes {@link #onConnect} when a WebSocket client connects, providing a channel
 * for bidirectional communication.
 *
 * <p>Two callback instances are typically used: one for JSON-RPC text messages and one for binary
 * protocol messages.
 *
 * @see YjsChannel
 */
public interface YjsChannelCallbacks {

  /**
   * Called when a new channel is established.
   *
   * <p>Implementations should subscribe to the channel to receive messages and may send initial
   * messages to establish the protocol.
   *
   * @param channel the newly connected channel
   */
  void onConnect(YjsChannel channel);
}

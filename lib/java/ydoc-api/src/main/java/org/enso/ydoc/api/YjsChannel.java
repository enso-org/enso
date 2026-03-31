package org.enso.ydoc.api;

import java.util.function.Consumer;

/**
 * A bidirectional communication channel between the Language Server and the Ydoc server.
 *
 * <p>Register <em>JSON</em> and/or <em>binary</em> implementations of {@link Server} when
 * initializing the Ydoc subsystem - e.g. when calling {@link YdocServerApi#launchYdocServer}
 *
 * <p>Whenever new connection arrives, a call to {@link
 * Server#onConnect(org.enso.ydoc.api.YjsChannel)} method is made with a provided instance of the
 * appropriate {@link YjsChannel} that can be used for communication. Either to {@link #send}
 * message, or by {@link #subscribe subscribing} to receive messages.
 *
 * @see Server
 */
public interface YjsChannel {

  /**
   * Sends a message through the channel to the remote endpoint.
   *
   * @param message the message to send (typically String for JSON or ByteBuffer for binary)
   */
  void send(Object message);

  /**
   * Subscribes to receive messages from the remote endpoint.
   *
   * <p>Messages sent by this endpoint are automatically filtered out. If messages arrived before
   * subscription, they will be delivered immediately upon subscribing.
   *
   * @param messageHandler callback invoked for each incoming message
   */
  void subscribe(Consumer<Object> messageHandler);

  /**
   * Callback interface for receiving newly established {@link YjsChannel} connections.
   *
   * <p>Implementations handle the lifecycle of channels between the Language Server and Ydoc
   * server. The Ydoc server invokes {@link #onConnect} when a WebSocket client connects, providing
   * a channel for bidirectional communication.
   *
   * <p>Two callback instances are typically used: one for JSON-RPC text messages and one for binary
   * protocol messages.
   *
   * @see YjsChannel
   */
  public interface Server {

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
}

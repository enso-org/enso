package org.enso.ydoc.api;

import java.util.function.Consumer;

/**
 * A bidirectional communication channel between the Language Server and the Ydoc server.
 *
 * <p>Register <em>JSON</em> and/or <em>binary</em> implementations of {@link Server} when
 * initializing the Ydoc subsystem - e.g. when calling {@link
 * YdocServerApi#launchYdocServer(java.lang.String, int, org.enso.ydoc.api.YjsChannelCallbacks,
 * org.enso.ydoc.api.YjsChannelCallbacks, org.slf4j.event.Level)}
 *
 * <p>Whenever new connection arrives, a call to {@link
 * Server#onConnect(org.enso.ydoc.api.YjsChannel)} method is made with a provided instance of the
 * appropriate {@link YjsChannel} that can be used for communication. Either to {@link #send}
 * message, or by {@link #subscribe subscribing} to receive messages.
 *
 * @param <M> the type of message this channel operates on
 * @see Server
 */
public final class YjsChannel<M> {
  private final Consumer<M> send;
  private final Consumer<Consumer<M>> subscribe;

  private YjsChannel(Consumer<M> send, Consumer<Consumer<M>> subscribe) {
    this.send = send;
    this.subscribe = subscribe;
  }

  /**
   * Sends a message through the channel to the remote endpoint.
   *
   * @param message the message to send (typically String for JSON or ByteBuffer for binary)
   */
  public void send(M message) {
    send.accept(message);
  }

  /**
   * Subscribes to receive messages from the remote endpoint.
   *
   * <p>Messages sent by this endpoint are automatically filtered out. If messages arrived before
   * subscription, they will be delivered immediately upon subscribing.
   *
   * @param messageHandler callback invoked for each incoming message
   */
  public void subscribe(Consumer<M> messageHandler) {
    subscribe.accept(messageHandler);
  }

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
   * @param <M> the type of message that's sent via the
   * @see YjsChannel
   */
  public interface Server<M> {

    /**
     * Called when a new channel is established.
     *
     * <p>Implementations should subscribe to the channel to receive messages and may send initial
     * messages to establish the protocol.
     *
     * @param channel the newly connected channel
     */
    void onConnect(YjsChannel<M> channel);
  }

  /**
   * Service provider factory method for a new channel.
   *
   * @param <M>
   * @param send implementation to call when sending a message
   * @param subscribe implementation to call when subscribing for a message
   * @return
   */
  public static <M> YjsChannel<M> create(Consumer<M> send, Consumer<Consumer<M>> subscribe) {
    return new YjsChannel<>(send, subscribe);
  }
}

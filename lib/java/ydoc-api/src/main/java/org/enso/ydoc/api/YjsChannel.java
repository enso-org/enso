package org.enso.ydoc.api;

import java.util.function.Consumer;

/**
 * A bidirectional communication channel between the Language Server and the Ydoc server.
 *
 * <p>This interface abstracts message passing between different runtime environments (Java/Scala
 * and JavaScript/TypeScript) in the Enso IDE architecture. Channels are created when WebSocket
 * clients connect to the Ydoc server and are delivered via {@link YjsChannelCallbacks#onConnect}.
 *
 * @see YjsChannelCallbacks
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
}

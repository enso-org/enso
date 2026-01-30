package org.enso.ydoc.server;

import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;

/**
 * Thread-safe {@link YjsChannel} wrapper that delegates operations to the Ydoc executor thread.
 *
 * <p>GraalJS polyglot context requires all JavaScript interactions to occur on a single thread.
 * This wrapper queues channel operations to the {@link YdocScheduledExecutorService} to satisfy
 * this constraint.
 */
public final class YjsChannelSynchronized implements YjsChannel {

  private final YjsChannel channel;
  private final YdocScheduledExecutorService executor;

  /**
   * @param channel the underlying channel to wrap
   * @param executor the Ydoc executor that owns the GraalJS context thread
   */
  public YjsChannelSynchronized(YjsChannel channel, YdocScheduledExecutorService executor) {
    this.channel = channel;
    this.executor = executor;
  }

  /** Queues the message to be sent on the Ydoc executor thread. */
  @Override
  public void send(Object message) {
    executor.submit(() -> channel.send(message));
  }

  /** Queues the subscription to be registered on the Ydoc executor thread. */
  @Override
  public void subscribe(Consumer<Object> messageHandler) {
    executor.submit(() -> channel.subscribe(messageHandler));
  }
}

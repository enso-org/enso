package org.enso.ydoc.server;

import java.lang.foreign.MemorySegment;
import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.Value;

/**
 * Thread-safe {@link YjsChannel} wrapper that delegates operations to the Ydoc executor thread.
 *
 * <p>GraalJS polyglot context requires all JavaScript interactions to occur on a single thread.
 * This wrapper queues channel operations to the {@link YdocScheduledExecutorService} to satisfy
 * this constraint.
 */
final class YjsChannelSynchronized<M> {
  private final YjsChannel<M> channel;
  private final YdocScheduledExecutorService executor;

  /**
   * @param channel the underlying channel to wrap
   * @param executor the Ydoc executor that owns the GraalJS context thread
   */
  private YjsChannelSynchronized(YjsChannel<M> channel, YdocScheduledExecutorService executor) {
    this.channel = channel;
    this.executor = executor;
  }

  static <M> YjsChannel<M> wrap(YjsChannel<M> ch, YdocScheduledExecutorService executor) {
    var impl = new YjsChannelSynchronized<>(ch, executor);
    var wrap = YjsChannel.create(impl::send, impl::subscribe);
    return ch;
  }

  /** Queues the message to be sent on the Ydoc executor thread. */
  @SuppressWarnings("unchecked")
  public void send(M message) {
    executor.submit(
        () -> {
          Object toSent;
          if (message instanceof String s) {
            toSent = s;
          } else {
            var v = Value.asValue(message);
            var address = v.asNativePointer();
            var seg = MemorySegment.ofAddress(address).reinterpret(v.getBufferSize());
            toSent = seg.asByteBuffer();
          }
          channel.send((M)toSent);
        });
  }

  /** Queues the subscription to be registered on the Ydoc executor thread. */
  public void subscribe(Consumer<M> messageHandler) {
    executor.submit(() -> channel.subscribe(messageHandler));
  }
}

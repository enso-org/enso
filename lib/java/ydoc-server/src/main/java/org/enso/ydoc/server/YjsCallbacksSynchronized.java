package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.HostAccess;

/**
 * Thread-safe {@link YjsChannel.Server} wrapper that provides synchronized channels to delegates.
 *
 * <p>GraalJS polyglot context requires all JavaScript interactions to occur on a single thread.
 * When a connection is established, this wrapper creates a {@link YjsChannelSynchronized} around
 * the raw channel before passing it to the delegate, ensuring the Language Server can safely
 * interact with channels from any thread.
 */
final class YjsCallbacksSynchronized implements YjsChannel.Server {

  private final YjsChannel.Server callbacks;
  private final YdocScheduledExecutorService executor;

  /**
   * @param callbacks the delegate to receive synchronized channels
   * @param executor the Ydoc executor that owns the GraalJS context thread
   */
  YjsCallbacksSynchronized(YjsChannel.Server callbacks, YdocScheduledExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  /**
   * Wraps the channel in {@link YjsChannelSynchronized} and forwards to the delegate. A null
   * delegate is treated as a no-op so a {@link Ydoc} configured without callbacks for a given
   * channel does not crash the JS side when a connection arrives.
   */
  @Override
  @HostAccess.Export
  public void onConnect(YjsChannel channel) {
    if (this.callbacks == null) {
      return;
    }
    var synchronizedChannel = new YjsChannelSynchronized(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

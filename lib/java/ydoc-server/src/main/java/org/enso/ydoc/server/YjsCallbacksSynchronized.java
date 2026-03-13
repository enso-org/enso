package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.HostAccess;

/**
 * Thread-safe {@link YjsChannelCallbacks} wrapper that provides synchronized channels to delegates.
 *
 * <p>GraalJS polyglot context requires all JavaScript interactions to occur on a single thread.
 * When a connection is established, this wrapper creates a {@link YjsChannelSynchronized} around
 * the raw channel before passing it to the delegate, ensuring the Language Server can safely
 * interact with channels from any thread.
 */
final class YjsCallbacksSynchronized<M> implements YjsChannel.Server<M> {

  private final YjsChannel.Server<M> callbacks;
  private final YdocScheduledExecutorService executor;

  /**
   * @param callbacks the delegate to receive synchronized channels
   * @param executor the Ydoc executor that owns the GraalJS context thread
   */
  YjsCallbacksSynchronized(YjsChannel.Server<M> callbacks, YdocScheduledExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  /** Wraps the channel in {@link YjsChannelSynchronized} and forwards to the delegate. */
  @Override
  @HostAccess.Export
  public void onConnect(YjsChannel<M> channel) {
    var synchronizedChannel = YjsChannelSynchronized.wrap(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

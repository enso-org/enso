package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.api.YjsChannelCallbacks;
import org.graalvm.polyglot.HostAccess;

/**
 * Thread-safe {@link YjsChannelCallbacks} wrapper that provides synchronized channels to delegates.
 *
 * <p>GraalJS polyglot context requires all JavaScript interactions to occur on a single thread.
 * When a connection is established, this wrapper creates a {@link YjsChannelSynchronized} around
 * the raw channel before passing it to the delegate, ensuring the Language Server can safely
 * interact with channels from any thread.
 */
public final class YjsCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final YdocScheduledExecutorService executor;

  /**
   * @param callbacks the delegate to receive synchronized channels
   * @param executor the Ydoc executor that owns the GraalJS context thread
   */
  YjsCallbacksSynchronized(YjsChannelCallbacks callbacks, YdocScheduledExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  /** Wraps the channel in {@link YjsChannelSynchronized} and forwards to the delegate. */
  @Override
  @HostAccess.Export
  public void onConnect(YjsChannel channel) {
    var synchronizedChannel = new YjsChannelSynchronized(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

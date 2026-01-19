package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.api.YjsChannelCallbacks;

public final class YjsCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final YdocScheduledExecutorService executor;

  public YjsCallbacksSynchronized(
      YjsChannelCallbacks callbacks, YdocScheduledExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  @Override
  public void onConnect(YjsChannel channel) {
    var synchronizedChannel = new YjsChannelSynchronized(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

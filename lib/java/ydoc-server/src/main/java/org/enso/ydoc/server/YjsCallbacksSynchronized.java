package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.api.YjsChannelCallbacks;

public final class YjsCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final YdocScheduledExecutorService executor;

  YjsCallbacksSynchronized(YjsChannelCallbacks callbacks, YdocScheduledExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  @Override
  public void onConnect(YjsChannel channel) {
    System.out.println("YjsCallbacksSynchronized.onConnect " + channel.getClass());
    var synchronizedChannel = new YjsChannelSynchronized(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

package org.enso.ydoc.server;

import java.util.concurrent.ExecutorService;
import org.enso.ydoc.api.YjsChannelCallbacks;
import org.enso.ydoc.api.YjsChannel;

public final class YjsCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final ExecutorService executor;

  public YjsCallbacksSynchronized(YjsChannelCallbacks callbacks, ExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  @Override
  public void onConnect(YjsChannel channel) {
    var synchronizedChannel = new YjsChannelSynchronized(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

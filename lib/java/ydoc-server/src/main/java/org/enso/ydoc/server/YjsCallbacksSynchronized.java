package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.api.YjsChannelCallbacks;
import org.graalvm.polyglot.HostAccess;

public final class YjsCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final YdocScheduledExecutorService executor;

  YjsCallbacksSynchronized(YjsChannelCallbacks callbacks, YdocScheduledExecutorService executor) {
    this.callbacks = callbacks;
    this.executor = executor;
  }

  @Override
  @HostAccess.Export
  public void onConnect(YjsChannel channel) {
    var synchronizedChannel = new YjsChannelSynchronized(channel, this.executor);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

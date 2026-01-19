package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.api.YjsChannelCallbacks;
import org.graalvm.polyglot.Context;

public final class YjsBinaryChannelCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final YdocScheduledExecutorService executor;
  private final Context context;

  public YjsBinaryChannelCallbacksSynchronized(
      YjsChannelCallbacks callbacks, YdocScheduledExecutorService executor, Context context) {
    this.callbacks = callbacks;
    this.executor = executor;
    this.context = context;
  }

  @Override
  public void onConnect(YjsChannel channel) {
    var synchronizedChannel =
        new YjsBinaryChannelSynchronized(channel, this.executor, this.context);
    this.callbacks.onConnect(synchronizedChannel);
  }
}

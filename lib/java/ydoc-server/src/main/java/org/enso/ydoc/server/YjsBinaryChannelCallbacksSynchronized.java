package org.enso.ydoc.server;

import java.util.concurrent.ExecutorService;
import org.enso.ydoc.api.YjsChannelCallbacks;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.Context;

public final class YjsBinaryChannelCallbacksSynchronized implements YjsChannelCallbacks {

  private final YjsChannelCallbacks callbacks;
  private final ExecutorService executor;
  private final Context context;

  public YjsBinaryChannelCallbacksSynchronized(
      YjsChannelCallbacks callbacks, ExecutorService executor, Context context) {
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

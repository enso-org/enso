package org.enso.ydoc.server;

import java.util.concurrent.ExecutorService;
import org.enso.ydoc.api.MessageCallbacks;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.Context;

public class YjsBinaryChannelCallbacksSynchronized implements MessageCallbacks {

  private final MessageCallbacks callbacks;
  private final ExecutorService executor;
  private final Context context;

  public YjsBinaryChannelCallbacksSynchronized(
      MessageCallbacks callbacks, ExecutorService executor, Context context) {
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

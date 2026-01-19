package org.enso.ydoc.server;

import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.Context;

public final class YjsBinaryChannelSynchronized implements YjsChannel {

  private final YjsChannel channel;
  private final YdocScheduledExecutorService executor;
  private final Context context;

  public YjsBinaryChannelSynchronized(
      YjsChannel channel, YdocScheduledExecutorService executor, Context context) {
    this.channel = channel;
    this.executor = executor;
    this.context = context;
  }

  @Override
  public void send(Object message) {
    executor.submit(() -> channel.send(message));
  }

  @Override
  public void subscribe(Consumer<Object> messageHandler) {
    executor.submit(
        () ->
            channel.subscribe(
                (message) -> {
                  var value = context.asValue(message);
                  var bytes = value.as(byte[].class);
                  messageHandler.accept(bytes);
                }));
  }
}

package org.enso.ydoc.server;

import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.Context;

public final class YjsBinaryChannelSynchronized implements YjsChannel {

  private final YjsChannel channel;
  private final ExecutorService executor;
  private final Context context;

  public YjsBinaryChannelSynchronized(
      YjsChannel channel, ExecutorService executor, Context context) {
    this.channel = channel;
    this.executor = executor;
    this.context = context;
  }

  @Override
  public void send(Object message) {
    executor.execute(() -> channel.send(message));
  }

  @Override
  public void subscribe(Consumer<Object> messageHandler) {
    executor.execute(
        () ->
            channel.subscribe(
                (message) -> {
                  var value = context.asValue(message);
                  var bytes = value.as(byte[].class);
                  messageHandler.accept(bytes);
                }));
  }
}

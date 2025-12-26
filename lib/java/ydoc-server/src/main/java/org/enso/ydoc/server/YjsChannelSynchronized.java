package org.enso.ydoc.server;

import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;

public class YjsChannelSynchronized implements YjsChannel {

  private final YjsChannel channel;
  private final ExecutorService executor;

  public YjsChannelSynchronized(YjsChannel channel, ExecutorService executor) {
    this.channel = channel;
    this.executor = executor;
  }

  @Override
  public void send(Object message) {
    executor.execute(() -> channel.send(message));
  }

  @Override
  public void subscribe(Consumer<Object> messageHandler) {
    executor.execute(() -> channel.subscribe(messageHandler));
  }
}

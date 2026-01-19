package org.enso.ydoc.server;

import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;

public final class YjsChannelSynchronized implements YjsChannel {

  private final YjsChannel channel;
  private final YdocScheduledExecutorService executor;

  public YjsChannelSynchronized(YjsChannel channel, YdocScheduledExecutorService executor) {
    this.channel = channel;
    this.executor = executor;
  }

  @Override
  public void send(Object message) {
    executor.submit(() -> channel.send(message));
  }

  @Override
  public void subscribe(Consumer<Object> messageHandler) {
    System.out.println("YjsChannelSynchronized.subscribe " + messageHandler.getClass());
    executor.submit(() -> {
      try {
        channel.subscribe(messageHandler);
      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("YjsChannelSynchronized.subscribe ERR " + e);
      }
    });
  }
}

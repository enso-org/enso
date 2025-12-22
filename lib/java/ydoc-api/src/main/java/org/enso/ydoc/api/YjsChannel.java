package org.enso.ydoc.api;

import java.util.function.Consumer;

public interface YjsChannel {
  public void send(Object message);

  public void subscribe(Consumer<Object> messageHandler);
}

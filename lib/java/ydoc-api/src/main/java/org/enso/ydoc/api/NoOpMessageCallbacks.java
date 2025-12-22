package org.enso.ydoc.api;

public class NoOpMessageCallbacks implements MessageCallbacks {
  public static final NoOpMessageCallbacks INSTANCE = new NoOpMessageCallbacks();

  private NoOpMessageCallbacks() {}

  @Override
  public void onConnect(YjsChannel channel) {}
}

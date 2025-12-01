package org.enso.ydoc.api;

public interface MessageCallbacks {
  public void sendText(String message);

  public void onText(String message);
}

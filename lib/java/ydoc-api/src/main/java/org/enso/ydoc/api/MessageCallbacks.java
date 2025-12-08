package org.enso.ydoc.api;

public interface MessageCallbacks {
  public interface YjsChannel {
    public void sendText(String message);
  }

  public void onConnect(YjsChannel channel);

  public void onText(String message);
}

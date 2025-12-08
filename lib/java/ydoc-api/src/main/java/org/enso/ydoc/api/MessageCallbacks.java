package org.enso.ydoc.api;

public interface MessageCallbacks {

  public void onConnect(YjsChannel channel);

  public void onMessage(Object message);
}

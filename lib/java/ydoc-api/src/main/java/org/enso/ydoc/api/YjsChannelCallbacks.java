package org.enso.ydoc.api;

public interface YjsChannelCallbacks {

  public void onConnect(YjsChannel channel);

  public default void test(YjsChannel channel) {
    System.out.println("TEST CALLBACK " + channel.getClass());
  }
}

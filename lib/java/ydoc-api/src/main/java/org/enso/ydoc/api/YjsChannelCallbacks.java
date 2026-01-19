package org.enso.ydoc.api;

public interface YjsChannelCallbacks {

  public void onConnect(YjsChannel channel);

  default public void test(YjsChannel channel) {
    System.out.println("TEST CALLBACK " + channel.getClass());
  }
}

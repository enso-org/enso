package org.enso.ht;

import io.helidon.webserver.WebServer;

public class Main {

  private Main() {}

  public static void main(String[] args) throws Exception {
    System.setProperty(
        "helidon.serialFilter.pattern",
        "javax.management.**;java.lang.**;java.rmi.**;javax.security.auth.Subject;!*");

    var server = WebServer.builder().host("localhost").port(1235).build().start();

    System.out.println("WebServer started.");
  }
}

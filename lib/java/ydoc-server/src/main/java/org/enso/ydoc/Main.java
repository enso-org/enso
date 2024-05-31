package org.enso.ydoc;

import java.util.concurrent.Semaphore;

public class Main {

  private static final String YDOC_HOST="YDOC_HOST";
  private static final String YDOC_PORT="YDOC_PORT";

  private static final Semaphore lock = new Semaphore(0);

  private Main() {}

  public static void main(String[] args) throws Exception {
    System.setProperty(
        "helidon.serialFilter.pattern",
        "javax.management.**;java.lang.**;java.rmi.**;javax.security.auth.Subject;!*");

    Sampling.init();

    var ydocHost = System.getenv(YDOC_HOST);
    var ydocPort = System.getenv(YDOC_PORT);

    var builder = Ydoc.builder();
    if (ydocHost != null) {
      builder.hostname(ydocHost);
    }
    if (ydocPort != null) {
      var port = Integer.parseInt(ydocPort);
      builder.port(port);
    }

    try (var ydoc = builder.build()) {
      ydoc.start();
      lock.acquire();
    }
  }
}

package org.enso.ydoc.server;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Semaphore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class Main {
  private static final Logger log = LoggerFactory.getLogger(Main.class);

  private static final String ENSO_YDOC_HOST = "ENSO_YDOC_HOST";
  private static final String ENSO_YDOC_PORT = "ENSO_YDOC_PORT";

  private static final Semaphore lock = new Semaphore(0);

  private Main() {}

  public static void main(String[] args) throws Exception {
    System.setProperty(
        "helidon.serialFilter.pattern",
        "javax.management.**;java.lang.**;java.rmi.**;javax.security.auth.Subject;!*");

    if (args.length == 2) {
      var then = System.currentTimeMillis();
      var hostname = args[0];
      var port = args[1];
      launch(hostname, port);

      var now = System.currentTimeMillis();
      var took = now - then;
      log.debug("Ydoc server at {}:{} started in {} ms", hostname, port, took);
    } else {
      var hostname = System.getenv(ENSO_YDOC_HOST);
      var port = System.getenv(ENSO_YDOC_PORT);
      try (var ydoc = launch(hostname, port)) {
        lock.acquire();
      }
    }
  }

  private static AutoCloseable launch(String ydocHost, String ydocPort) throws IOException {
    try {
      var builder = Ydoc.builder();
      if (ydocHost != null) {
        builder.hostname(ydocHost);
      }
      if (ydocPort != null) {
        var port = Integer.parseInt(ydocPort);
        builder.port(port);
      }
      var ydoc = builder.build();
      ydoc.start();
      return ydoc;
    } catch (ExecutionException | InterruptedException ex) {
      throw new IOException(ex);
    }
  }
}

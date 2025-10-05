package org.enso.ydoc.server;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Launched by ydoc-server-registration module in the other JVM. */
public final class DualMain {
  private static final Logger log = LoggerFactory.getLogger(DualMain.class);

  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      throw new IOException("Usage: java org.enso.ydoc.server.DualMain hostname port");
    }
    var hostname = args[0];
    var port = Integer.parseInt(args[1]);
    launch(hostname, port);
  }

  private static AutoCloseable launch(String hostname, int port) throws IOException {
    try {
      var then = System.currentTimeMillis();
      var ydoc = Ydoc.builder().hostname(hostname).port(port).build();
      ydoc.start();
      var now = System.currentTimeMillis();
      log.warn("Ydoc server at {}:{} started in {} ms", hostname, port, now - then);
      System.err.printf("Ydoc server at %s:%d started in %d ms\n", hostname, port, now - then);
      return ydoc;
    } catch (ExecutionException | InterruptedException ex) {
      throw new IOException(ex);
    }
  }
}

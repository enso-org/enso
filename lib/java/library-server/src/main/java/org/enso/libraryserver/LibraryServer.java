package org.enso.libraryserver;

import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.file.Path;

/**
 * Simple server that serves static files from a directory. Is meant to be an Enso library
 * repository. Uses the built-in Java HTTP server.
 */
public final class LibraryServer implements AutoCloseable {
  private static final int STOP_DELAY_SECONDS = 2;
  private final int port;
  private final Path directory;
  private HttpServer server;

  public LibraryServer(int port, Path directory) {
    this.port = port;
    this.directory = directory;
  }

  public void start() {
    try {
      server = HttpServer.create(new InetSocketAddress(port), 0);
    } catch (IOException e) {
      throw new IllegalStateException("Failed to create server", e);
    }
    FileHandler fileHandler = null;
    try {
      fileHandler = new FileHandler(directory.toRealPath());
    } catch (IOException e) {
      throw new IllegalStateException("Failed to resolve directory path", e);
    }
    server.createContext("/", fileHandler);
    server.start();
  }

  @Override
  public void close() {
    stop();
  }

  private void stop() {
    server.stop(STOP_DELAY_SECONDS);
  }
}

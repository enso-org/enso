package org.enso.snowflake;

import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetSocketAddress;

public final class OAuthCallback {
  private OAuthCallback() {}

  public static CallbackServer waitForCallback(int port) throws IOException {
    var callbackServer = new CallbackServerImplementation(port);
    callbackServer.start();
  }

  public interface CallbackServer extends AutoCloseable {
    String waitForCallback();
  }

  private static final class CallbackServerImplementation implements CallbackServer {
    private final HttpServer server;

    private CallbackServerImplementation(int port) throws IOException {
      InetSocketAddress address = new InetSocketAddress("localhost", port);
      server = HttpServer.create(address, 0);
    }

    private void start() {
      server.start();
    }
  }
}

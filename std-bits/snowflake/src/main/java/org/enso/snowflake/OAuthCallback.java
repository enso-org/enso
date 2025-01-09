package org.enso.snowflake;

import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public final class OAuthCallback {
  private OAuthCallback() {}

  public static CallbackServer createCallbackServer(int port) throws IOException {
    var callbackServer = new CallbackServerImplementation(port);
    callbackServer.start();
    return callbackServer;
  }

  public interface CallbackServer extends AutoCloseable {
    String waitForCallback();
  }

  private static final class CallbackServerImplementation implements CallbackServer {
    private final HttpServer server;
    private final CompletableFuture<String> callbackResult = new CompletableFuture<>();

    private CallbackServerImplementation(int port) throws IOException {
      InetSocketAddress address = new InetSocketAddress("localhost", port);
      server = HttpServer.create(address, 0);
      server.createContext("snowflake", exchange -> {
        var query = exchange.getRequestURI().getQuery();
        byte[] response = OK_RESPONSE.getBytes();
        exchange.sendResponseHeaders(200, response.length);
        exchange.getResponseBody().write(response);
        exchange.close();
        callbackResult.complete(query);
      });
    }

    private void start() {
      server.start();
    }

    @Override
    public String waitForCallback() {
      try {
        return callbackResult.get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }
    }

    @Override
    public void close() throws Exception {
      server.stop(1);
    }

    private static final String OK_RESPONSE =
        """
        <html>
          <head>
            <title>Enso - Snowflake integration</title>
            <style>
            body {
              display: flex;
              justify-content: center;
              align-items: center;
              height: 100vh;
              margin: 0;
            }
            </style>
          </head>
          <body>
            <h1>Enso - Snowflake integration</h1>
            <p>OAuth callback received. You can close this window now and go back to the application.</p>
          </body>
        </html>
        """;
  }
}

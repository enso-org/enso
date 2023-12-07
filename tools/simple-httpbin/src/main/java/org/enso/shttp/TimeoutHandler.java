package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

public class TimeoutHandler implements HttpHandler {
  private final int timeoutMillis;

  public TimeoutHandler(int timeoutMillis) {
    this.timeoutMillis = timeoutMillis;
  }

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    byte[] responsePart = "OK".getBytes(StandardCharsets.UTF_8);
    int responseCount = 2000;

    exchange.sendResponseHeaders(200, (long) responsePart.length * responseCount);

    try (OutputStream os = exchange.getResponseBody()) {
      int half = responseCount / 2;
      for (int i = 0; i < half; i++) {
        os.write(responsePart);
      }
      os.flush();
      Thread.sleep(timeoutMillis);

      for (int i = half; i < responseCount; i++) {
        os.write(responsePart);
      }
    } catch (InterruptedException e) {
      System.out.println("Interrupted");
      e.printStackTrace();
    }
  }
}

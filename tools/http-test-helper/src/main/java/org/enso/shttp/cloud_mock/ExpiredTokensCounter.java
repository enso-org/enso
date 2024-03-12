package org.enso.shttp.cloud_mock;

import com.sun.net.httpserver.HttpExchange;
import org.enso.shttp.SimpleHttpHandler;

import java.io.IOException;

/**
 * A special endpoint that returns the number of requests which failed due to providing an expired token.
 * This is used in tests, to verify that requests are indeed failed and retried, and not just successful on first try.
 */
public class ExpiredTokensCounter extends SimpleHttpHandler {
  private long counter = 0;

  void registerExpiredTokenFailure() {
    counter++;
  }

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    sendResponse(200, Long.toString(counter), exchange);
  }
}

package org.enso.shttp.test_helpers;

import com.sun.net.httpserver.HttpExchange;
import org.enso.shttp.SimpleHttpHandler;

import java.io.IOException;

public class RedirectTestHandler extends SimpleHttpHandler {
  private final String redirectLocation;

  public RedirectTestHandler(String redirectLocation) {
    this.redirectLocation = redirectLocation;
  }

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    exchange.getResponseHeaders().add("Location", redirectLocation);
    exchange.sendResponseHeaders(302, -1);
  }
}

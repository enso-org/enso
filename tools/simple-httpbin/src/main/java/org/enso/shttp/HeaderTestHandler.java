package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.net.URI;
import org.apache.http.client.utils.URIBuilder;

public class HeaderTestHandler extends SimpleHttpHandler {
  @Override
  public void doHandle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);
    try {
      for (var queryPair : builder.getQueryParams()) {
        exchange.getResponseHeaders().add(queryPair.getName(), queryPair.getValue());
      }
    } catch (Exception e) {
      e.printStackTrace();
      exchange.sendResponseHeaders(500, -1);
    }

    exchange.sendResponseHeaders(200, -1);
  }
}

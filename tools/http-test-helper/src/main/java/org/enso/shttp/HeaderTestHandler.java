package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;

import org.apache.http.client.utils.URIBuilder;

public class HeaderTestHandler extends SimpleHttpHandler {
  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);

    String content = null;

    try {
      for (var queryPair : builder.getQueryParams()) {
        if (queryPair.getName().equals("content")) {
          content = queryPair.getValue();
        } else {
          exchange.getResponseHeaders().add(queryPair.getName(), queryPair.getValue());
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
      exchange.sendResponseHeaders(500, -1);
    }

    if (content != null) {
      byte[] response = content.getBytes(StandardCharsets.UTF_8);
      exchange.sendResponseHeaders(200, response.length);
      try (OutputStream os = exchange.getResponseBody()) {
        os.write(response);
      }
      exchange.close();
    } else {
      exchange.sendResponseHeaders(200, -1);
    }
  }
}

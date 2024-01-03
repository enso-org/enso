package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import org.apache.http.client.utils.URIBuilder;

public class HeaderTestHandler extends SimpleHttpHandler {
  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);

    final String responseDataArgumentName = "base64_response_data";
    byte[] responseData = null;

    try {
      for (var queryPair : builder.getQueryParams()) {
        if (queryPair.getName().equals(responseDataArgumentName)) {
          responseData = java.util.Base64.getDecoder().decode(queryPair.getValue());
        } else {
          exchange.getResponseHeaders().add(queryPair.getName(), queryPair.getValue());
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
      exchange.sendResponseHeaders(500, -1);
    }

    if (responseData != null) {
      exchange.sendResponseHeaders(200, responseData.length);
      try (OutputStream os = exchange.getResponseBody()) {
        os.write(responseData);
      }
      exchange.close();
    } else {
      exchange.sendResponseHeaders(200, -1);
    }
  }
}

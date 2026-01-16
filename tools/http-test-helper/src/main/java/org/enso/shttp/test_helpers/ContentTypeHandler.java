package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.net.URI;
import java.util.Objects;
import org.apache.http.client.utils.URIBuilder;
import org.enso.shttp.SimpleHttpHandler;

/** An endpoint for returning a dummy body with a particular content type */
public class ContentTypeHandler extends SimpleHttpHandler {
  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    System.out.println("HI " + exchange.getRequestURI());
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);
    String contentType = "text/plain";
    for (var queryPair : builder.getQueryParams()) {
      switch (queryPair.getName()) {
        case "content-type" -> contentType = queryPair.getValue();
        default -> {}
      }
    }
    sendResponse(200, "hello", exchange, contentType);
  }
}

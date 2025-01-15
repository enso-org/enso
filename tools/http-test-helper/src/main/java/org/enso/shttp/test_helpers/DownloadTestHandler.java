package org.enso.shttp.test_helpers;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.Random;
import org.apache.http.client.utils.URIBuilder;
import org.enso.shttp.SimpleHttpHandler;

/**
 * A handler that generates a data response, with optional max-age and Age headers. The data
 * response consists of a string of random letters of the requested length.
 */
public class DownloadTestHandler extends SimpleHttpHandler {
  private Random random = new Random(42);

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);

    int length = 10;
    String maxAge = null;
    String age = null;
    boolean omitContentLength = false;
    for (var queryPair : builder.getQueryParams()) {
      switch (queryPair.getName()) {
        case "length" -> length = Integer.parseInt(queryPair.getValue());
        case "max-age" -> maxAge = queryPair.getValue();
        case "age" -> age = queryPair.getValue();
        case "omit-content-length" -> omitContentLength = true;
        default -> {}
      }
    }

    byte responseData[] = new byte[length];
    for (int i = 0; i < length; ++i) {
      responseData[i] = (byte) (97 + random.nextInt(26));
    }

    if (maxAge != null) {
      exchange.getResponseHeaders().add("Cache-Control", "max-age=" + maxAge);
    }

    if (age != null) {
      exchange.getResponseHeaders().add("Age", age.toString());
    }

    long contentLength = omitContentLength ? 0 : responseData.length;
    exchange.sendResponseHeaders(200, contentLength);

    try (OutputStream os = exchange.getResponseBody()) {
      os.write(responseData);
    }
    exchange.close();
  }
}

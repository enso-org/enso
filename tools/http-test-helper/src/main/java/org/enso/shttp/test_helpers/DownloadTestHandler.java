package org.enso.shttp.test_helpers;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.Arrays;
import org.apache.http.client.utils.URIBuilder;
import org.enso.shttp.SimpleHttpHandler;

/** A handler that generates a data response, with optional max-age and Age headers. */
public class DownloadTestHandler extends SimpleHttpHandler {
  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);

    int length = 10;
    String maxAge = null;
    String age = null;
    boolean omitContentLength = false;
    for (var queryPair : builder.getQueryParams()) {
      if (queryPair.getName().equals("length")) {
        length = Integer.parseInt(queryPair.getValue());
      } else if (queryPair.getName().equals("max-age")) {
        maxAge = queryPair.getValue();
      } else if (queryPair.getName().equals("age")) {
        age = queryPair.getValue();
      } else if (queryPair.getName().equals("omit-content-length")) {
        omitContentLength = true;
      }
    }

    byte responseData[] = new byte[length];
    Arrays.fill(responseData, (byte) 97);

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

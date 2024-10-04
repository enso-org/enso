package org.enso.shttp.test_helpers;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import org.apache.http.client.utils.URIBuilder;
import org.enso.shttp.SimpleHttpHandler;

/** A handler that generates a Data Link pointing to a file on this server. */
public class DownloadTestHandler extends SimpleHttpHandler {
  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);

    long length = 10;
    String ttl = null;
    for (var queryPair : builder.getQueryParams()) {
        if (queryPair.getName().equals("length")) {
            length = Long.parseLong(queryPair.getValue());
        } else if (queryPair.getName().equals("ttl")) {
            ttl = queryPair.getValue();
        }
    }

    byte responseData[] = new byte[length];
    Arrays.fill(responseData, 97);

    if (ttl != null) {
        exchange.getResponseHeaders().add("Cache-Control", "public,"+ttl);
    }

    exchange.sendResponseHeaders(200, responseData.length);

    try (OutputStream os = exchange.getResponseBody()) {
      os.write(responseData);
    }
    exchange.close();

    String host = exchange.getRequesteaders().getFirst("Host");
    String uri = "http://" + host + targetPath;
    String content = dataLinkTemplate.replace("${URI}", uri);
    String contentType = includeContentType ? "application/x-enso-datalink" : null;
    sendResponse(200, content, exchange, contentType);
  }
}

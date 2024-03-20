package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;

import java.io.IOException;

/** A handler that generates a Data Link pointing to a file on this server. */
public class GenerateDataLinkHandler extends SimpleHttpHandler {
  private final boolean includeContentType;
  private final static String targetPath = "/testfiles/js.txt";
  private final static String dataLinkTemplate = """
      {
          "type": "HTTP",
          "libraryName": "Standard.Base",
          "method": "GET",
          "uri": "${URI}"
      }
      """;

  public GenerateDataLinkHandler(boolean includeContentType) {
    this.includeContentType = includeContentType;
  }

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    String host = exchange.getRequestHeaders().getFirst("Host");
    String uri = "http://" + host + targetPath;
    String content = dataLinkTemplate.replace("${URI}", uri);
    if (includeContentType) {
      exchange.getResponseHeaders().add("Content-Type", "application/x-enso-datalink");
    }
    sendResponse(200, content, exchange);
  }
}

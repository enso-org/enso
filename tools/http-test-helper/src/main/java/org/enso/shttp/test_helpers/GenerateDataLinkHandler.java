package org.enso.shttp.test_helpers;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import org.enso.shttp.SimpleHttpHandler;

/** A handler that generates a Data Link pointing to a file on this server. */
public class GenerateDataLinkHandler extends SimpleHttpHandler {
  private final boolean includeContentType;
  private static final String targetPath = "/testfiles/js.txt";
  private static final String dataLinkTemplate =
      """
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
    String contentType = includeContentType ? "application/x-enso-datalink" : null;
    sendResponse(200, content, exchange, contentType);
  }
}

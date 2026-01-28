package org.enso.shttp.cloud_mock;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Path;
import org.apache.http.client.utils.URIBuilder;

/** An endpoint for returning a dummy body with a particular content type */
public class ContentTypeHandler implements HttpHandler {
  private final Path rootDir;

  public ContentTypeHandler(Path rootDir) {
    this.rootDir = rootDir;
  }

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    URIBuilder builder = new URIBuilder(uri);
    String contentType = getContentType(builder);

    File file = rootDir.resolve(builder.getPath().replaceFirst("/content_type/", "")).toFile();

    exchange.getResponseHeaders().add("Content-Type", contentType);
    exchange.sendResponseHeaders(200, file.length());

    try (FileInputStream fis = new FileInputStream(file);
        OutputStream os = exchange.getResponseBody()) {
      fis.transferTo(os);
    } finally {
      exchange.close();
    }
  }

  private String getContentType(URIBuilder builder) {
    String contentType = "text/plain";
    for (var queryPair : builder.getQueryParams()) {
      switch (queryPair.getName()) {
        case "content-type" -> contentType = queryPair.getValue();
        default -> {}
      }
    }
    return contentType;
  }
}

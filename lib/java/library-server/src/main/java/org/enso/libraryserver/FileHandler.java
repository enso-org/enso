package org.enso.libraryserver;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

final class FileHandler implements HttpHandler {
  private static final Map<String, String> MIME_MAP =
      Map.of(
          "yaml", "application/x-yaml",
          "tgz", "application/gzip",
          "json", "application/json");
  private static final String FALLBACK_MIME = "application/octet-stream";
  private static final Logger logger = Logger.getLogger(FileHandler.class.getName());

  private final Path directory;

  FileHandler(Path directory) {
    this.directory = directory;
  }

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    logRequest(exchange);
    try {
      if (!"GET".equalsIgnoreCase(exchange.getRequestMethod())
          && !"HEAD".equalsIgnoreCase(exchange.getRequestMethod())) {
        sendResponse(exchange, 405, "Method Not Allowed");
        return;
      }

      var uri = exchange.getRequestURI();
      var rawPath = uri.getRawPath();
      var decodedPath = URLDecoder.decode(rawPath, StandardCharsets.UTF_8);

      // remove leading slash
      if (decodedPath.startsWith("/")) {
        decodedPath = decodedPath.substring(1);
      }

      var requestedPath = directory.resolve(decodedPath).normalize();
      try {
        requestedPath = requestedPath.toRealPath();
      } catch (IOException e) {
        logger.fine("File not found: " + requestedPath);
        sendResponse(exchange, 404, "Not Found");
        return;
      }

      // Prevent path traversal
      if (!requestedPath.startsWith(directory)) {
        sendResponse(exchange, 403, "Forbidden");
        return;
      }

      if (Files.isDirectory(requestedPath)) {
        logger.warning("Requested path is a directory: " + requestedPath);
        sendResponse(exchange, 404, "Requested path is a directory: " + requestedPath);
        return;
      }

      if (!Files.exists(requestedPath) || !Files.isRegularFile(requestedPath)) {
        logger.fine("File not found: " + requestedPath);
        sendResponse(exchange, 404, "Not Found");
        return;
      }

      serveFile(exchange, requestedPath);
    } catch (IOException e) {
      logger.log(Level.SEVERE, "IO error while handling request", e);
      sendResponse(exchange, 500, "Internal Server Error");
    } finally {
      exchange.close();
    }
  }

  private void serveFile(HttpExchange exchange, Path file) throws IOException {
    var size = Files.size(file);
    var contentType = probeContentType(file);

    exchange.getResponseHeaders().set("Content-Type", contentType);

    // For HEAD requests send headers only
    if ("HEAD".equalsIgnoreCase(exchange.getRequestMethod())) {
      logger.fine(
          () ->
              String.format(
                  "HEAD request for file: %s, size=%d, content-type=%s", file, size, contentType));
      exchange.sendResponseHeaders(200, -1);
      return;
    }
    logger.fine(
        () -> String.format("Serving file: %s, size=%d, content-type=%s", file, size, contentType));

    exchange.sendResponseHeaders(200, size);
    try (var in = Files.newInputStream(file);
        var out = exchange.getResponseBody()) {
      byte[] buffer = new byte[8192];
      int read;
      while ((read = in.read(buffer)) != -1) {
        out.write(buffer, 0, read);
      }
    }
  }

  private String probeContentType(Path file) {
    var name = file.getFileName().toString();
    int i = name.lastIndexOf('.');
    if (i >= 0 && i + 1 < name.length()) {
      var ext = name.substring(i + 1).toLowerCase();
      return MIME_MAP.getOrDefault(ext, FALLBACK_MIME);
    }
    logger.warning("Could not determine file extension for MIME type: " + name);
    return FALLBACK_MIME;
  }

  private void sendResponse(HttpExchange exchange, int status, String message) throws IOException {
    byte[] bytes = message.getBytes(StandardCharsets.UTF_8);
    exchange.getResponseHeaders().set("Content-Type", "text/plain; charset=utf-8");
    exchange.sendResponseHeaders(status, bytes.length);
    try (OutputStream out = exchange.getResponseBody()) {
      out.write(bytes);
    }
  }

  private static void logRequest(HttpExchange exchange) {
    var method = exchange.getRequestMethod();
    var uri = exchange.getRequestURI();
    var headers = exchange.getRequestHeaders().entrySet();
    logger.fine(
        () ->
            String.format("Received request: method=%s, uri=%s, headers=%s", method, uri, headers));
  }
}

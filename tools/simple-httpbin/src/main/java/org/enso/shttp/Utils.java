package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class Utils {
  static void sendResponse(int code, String message, HttpExchange exchange) throws IOException {
    byte[] response = message.getBytes(StandardCharsets.UTF_8);
    exchange.getResponseHeaders().add("Content-Type", "text/plain; charset=utf-8");
    exchange.sendResponseHeaders(code, response.length);
    exchange.getResponseBody().write(response);
    exchange.close();
  }
}

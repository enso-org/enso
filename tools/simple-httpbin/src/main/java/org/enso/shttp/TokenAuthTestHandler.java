package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import java.io.IOException;
import java.util.List;

import static org.enso.shttp.Utils.sendResponse;

public class TokenAuthTestHandler implements HttpHandler {
  private final String secretToken = "deadbeef-coffee-1234";

  @Override
  public void handle(HttpExchange exchange) throws IOException {
    List<String> authHeaders = exchange.getRequestHeaders().get("Authorization");
    if (authHeaders.isEmpty()) {
      sendResponse(401, "Not authorized.", exchange);
      return;
    } else if (authHeaders.size() > 1) {
      sendResponse(400, "Ambiguous Authorization headers.", exchange);
      return;
    }

    String authHeader = authHeaders.get(0);
    String prefix = "Bearer ";
    if (!authHeader.startsWith(prefix)) {
      sendResponse(400, "Invalid authorization header format.", exchange);
      return;
    }

    String providedToken = authHeader.substring(prefix.length());
    boolean authorized = providedToken.equals(secretToken);
    if (!authorized) {
      sendResponse(403, "Invalid token.", exchange);
      return;
    }

    sendResponse(200, "Authorization successful.", exchange);
  }
}

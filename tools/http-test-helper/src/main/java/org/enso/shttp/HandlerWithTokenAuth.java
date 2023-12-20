package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.util.List;

public abstract class HandlerWithTokenAuth extends SimpleHttpHandler {
  protected abstract String getSecretToken();

  protected abstract void handleAuthorized(HttpExchange exchange) throws IOException;

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    List<String> authHeaders = exchange.getRequestHeaders().get("Authorization");
    if (authHeaders == null || authHeaders.isEmpty()) {
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
    boolean authorized = providedToken.equals(getSecretToken());
    if (!authorized) {
      sendResponse(403, "Invalid token.", exchange);
      return;
    }

    handleAuthorized(exchange);
  }
}

package org.enso.shttp.auth;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.util.List;
import org.enso.shttp.SimpleHttpHandler;

public abstract class HandlerWithTokenAuth extends SimpleHttpHandler {
  protected abstract boolean isTokenAllowed(String token);

  protected int getNoTokenStatus() {
    return 401;
  }

  protected int getInvalidTokenStatus(String token) {
    return 401;
  }

  protected abstract void handleAuthorized(HttpExchange exchange) throws IOException;

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    System.out.println("HTTP request at " + exchange.getRequestURI());

    List<String> authHeaders = exchange.getRequestHeaders().get("Authorization");
    if (authHeaders == null || authHeaders.isEmpty()) {
      sendResponse(getNoTokenStatus(), "Not authorized.", exchange);
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
    boolean authorized = isTokenAllowed(providedToken);
    if (!authorized) {
      sendResponse(getInvalidTokenStatus(providedToken), "Invalid token.", exchange);
      return;
    }

    handleAuthorized(exchange);
  }
}

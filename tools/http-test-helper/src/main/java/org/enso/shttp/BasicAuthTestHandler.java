package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.util.Base64;
import java.util.List;

public class BasicAuthTestHandler extends SimpleHttpHandler {
  private final String username = "enso-test-user";

  // ends with 😎 emoji
  private final String password = "my secret password: 1234@#; ść + \uD83D\uDE0E";

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
    String prefix = "Basic ";
    if (!authHeader.startsWith(prefix)) {
      sendResponse(400, "Invalid authorization header format.", exchange);
      return;
    }

    String encodedCredentials = authHeader.substring(prefix.length());
    String decodedCredentials = new String(Base64.getDecoder().decode(encodedCredentials));
    String[] credentials = decodedCredentials.split(":", 2);
    if (credentials.length != 2) {
      sendResponse(403, "Invalid authorization credential format.", exchange);
      return;
    }

    String providedUsername = credentials[0];
    String providedPassword = credentials[1];
    boolean authorized = providedUsername.equals(username) && providedPassword.equals(password);
    if (!authorized) {
      sendResponse(403, "Wrong username or password.", exchange);
      return;
    }

    sendResponse(200, "Authorization successful, welcome " + username + "!", exchange);
  }
}

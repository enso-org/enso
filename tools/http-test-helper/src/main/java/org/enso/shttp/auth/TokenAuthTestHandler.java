package org.enso.shttp.auth;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;

public class TokenAuthTestHandler extends HandlerWithTokenAuth {
  @Override
  protected boolean isTokenAllowed(String token) {
    return token.equals("deadbeef-coffee-1234");
  }

  @Override
  protected void handleAuthorized(HttpExchange exchange) throws IOException {
    sendResponse(200, "Authorization successful.", exchange);
  }
}

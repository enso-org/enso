package org.enso.shttp;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.util.List;

public class TokenAuthTestHandler extends HandlerWithTokenAuth {

  @Override
  protected String getSecretToken() {
    return "deadbeef-coffee-1234";
  }

  @Override
  protected void handleAuthorized(HttpExchange exchange) throws IOException {
    sendResponse(200, "Authorization successful.", exchange);
  }
}

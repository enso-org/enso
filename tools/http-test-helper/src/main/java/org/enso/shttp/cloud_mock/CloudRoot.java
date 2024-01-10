package org.enso.shttp.cloud_mock;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import org.enso.shttp.auth.HandlerWithTokenAuth;

public class CloudRoot extends HandlerWithTokenAuth {
  public final String prefix = "/enso-cloud-mock/";

  @Override
  protected String getSecretToken() {
    return "TEST-ENSO-TOKEN-caffee";
  }

  private final CloudHandler[] handlers =
      new CloudHandler[] {new UsersHandler(), new SecretsHandler()};

  @Override
  protected void handleAuthorized(HttpExchange exchange) throws IOException {
    URI uri = exchange.getRequestURI();
    String path = uri.getPath();
    int prefixStart = path.indexOf(prefix);
    if (prefixStart == -1) {
      sendResponse(400, "Invalid URI.", exchange);
      return;
    }

    String subPath = path.substring(prefixStart + prefix.length());
    for (CloudHandler handler : handlers) {
      if (handler.canHandle(subPath)) {
        handler.handleCloudAPI(wrapExchange(subPath, exchange));
        return;
      }
    }

    sendResponse(404, "No handler found for: " + subPath, exchange);
  }

  private CloudHandler.CloudExchange wrapExchange(String subPath, HttpExchange exchange) {
    return new CloudHandler.CloudExchange() {
      @Override
      public HttpExchange getHttpExchange() {
        return exchange;
      }

      @Override
      public String subPath() {
        return subPath;
      }

      @Override
      public void sendResponse(int code, String response) throws IOException {
        CloudRoot.this.sendResponse(code, response, exchange);
      }

      @Override
      public String decodeBodyAsText() throws IOException {
        return new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
      }
    };
  }
}

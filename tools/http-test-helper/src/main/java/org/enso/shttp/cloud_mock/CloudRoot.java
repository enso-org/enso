package org.enso.shttp.cloud_mock;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.net.URI;
import org.enso.shttp.HttpMethod;
import org.enso.shttp.auth.HandlerWithTokenAuth;

public class CloudRoot extends HandlerWithTokenAuth {
  public final String prefix = "/enso-cloud-mock/";

  private final ExpiredTokensCounter expiredTokensCounter;
  private final CloudHandler[] handlers;

  public CloudRoot(ExpiredTokensCounter expiredTokensCounter, CloudMockSetup setup) {
    this.expiredTokensCounter = expiredTokensCounter;
    AssetStore assetStore = new AssetStore();
    UsersService usersService = new UsersService();
    EventsService eventsService = new EventsService();
    this.handlers =
        new CloudHandler[] {
          new UsersHandler(usersService),
          new SecretsHandler(assetStore),
          new HiddenSecretsHandler(assetStore),
          new AssetsHandler(assetStore),
          new PathResolver(assetStore),
          new DirectoriesHandler(assetStore),
          new GetLogsHandler(eventsService),
          new PostLogHandler(usersService, eventsService, setup.logBatchingTestModeEnabled())
        };
  }

  @Override
  protected boolean isTokenAllowed(String token) {
    return token.equals("TEST-ENSO-TOKEN-caffee") || token.startsWith("TEST-RENEWED-");
  }

  @Override
  protected int getInvalidTokenStatus(String token) {
    boolean isValidButExpired = token.equals("TEST-EXPIRED-TOKEN-beef");
    if (isValidButExpired) {
      expiredTokensCounter.registerExpiredTokenFailure();
    }

    return 401;
  }

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

    System.err.println("No handler found for request: " + subPath);
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
      public void sendEmptyResponse(int code) throws IOException {
        CloudRoot.this.sendEmptyResponse(code, exchange);
      }

      @Override
      public String decodeBodyAsText() throws IOException {
        return CloudRoot.this.decodeBodyAsText(exchange);
      }

      @Override
      public HttpMethod getMethod() throws IllegalArgumentException {
        return HttpMethod.valueOf(exchange.getRequestMethod());
      }
    };
  }
}

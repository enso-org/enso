package org.enso.shttp.cloud_mock;

import java.io.IOException;
import org.enso.shttp.HttpMethod;

public class AssetsHandler implements CloudHandler {
  private static final String ASSETS = "assets";
  private final AssetStore assetStore;

  public AssetsHandler(AssetStore assetStore) {
    this.assetStore = assetStore;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(ASSETS);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    ;
    if (exchange.getMethod() == HttpMethod.DELETE) {
      String id = exchange.subPath().substring(ASSETS.length() + 1);
      boolean existed = assetStore.deleteSecret(id);
      if (existed) {
        exchange.sendResponse(200, "");
      } else {
        exchange.sendResponse(404, "Secret not found: " + id);
      }
    } else {
      exchange.sendResponse(
          405, "Method not allowed: " + exchange.getMethod() + " - mock only allows DELETE");
    }
  }
}

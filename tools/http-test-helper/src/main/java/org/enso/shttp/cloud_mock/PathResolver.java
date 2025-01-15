package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.regex.Pattern;

/** In the mock, only the user */
public class PathResolver implements CloudHandler {

  private static final String PATH_RESOLVE = "path/resolve";

  private final AssetStore assetStore;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public PathResolver(AssetStore assetStore) {
    this.assetStore = assetStore;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(PATH_RESOLVE);
  }

  private final Pattern pathPattern = Pattern.compile("enso://(.+?)");

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    String queryString = exchange.getHttpExchange().getRequestURI().getQuery().replace("+", " ");
    if (queryString == null) {
      exchange.sendResponse(400, "Missing `path` parameter in query string (empty).");
      return;
    }

    String prefix = "path=";
    if (!queryString.startsWith(prefix)) {
      exchange.sendResponse(400, "Missing `path` parameter in query string: `" + queryString + "`");
      return;
    }

    String path = queryString.substring(prefix.length());
    var matcher = pathPattern.matcher(path);
    if (!matcher.matches()) {
      exchange.sendResponse(400, "Invalid path: " + path);
      return;
    }

    String[] pathSegments = matcher.group(1).split("/");
    AssetStore.Asset foundAsset = assetStore.resolvePath(pathSegments);
    if (foundAsset == null) {
      exchange.sendResponse(404, "Asset not found: " + path);
      return;
    }

    String asJson = jsonMapper.writeValueAsString(foundAsset);
    exchange.sendResponse(200, asJson);
  }
}

package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.regex.Pattern;

public class PathResolver implements CloudHandler {

  private static final String PATH_RESOLVE = "path/resolve";

  private final String ORGANIZATION_NAME = "Test.ORG";

  private final AssetStore assetStore;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public PathResolver(AssetStore assetStore) {
    this.assetStore = assetStore;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(PATH_RESOLVE);
  }

  private final Pattern pathPattern = Pattern.compile("enso://(.+?)/(.+)");

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    String queryString = exchange.getHttpExchange().getRequestURI().getQuery();
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

    String organization = matcher.group(1);
    String subPath = matcher.group(2);

    if (!organization.equals(ORGANIZATION_NAME)) {
      exchange.sendResponse(404, "Organization not found: " + organization);
      return;
    }

    // The latter condition is a workaround for https://github.com/enso-org/cloud-v2/issues/1173 and
    // it may be removed once that is fixed
    boolean isRoot = subPath.isEmpty() || subPath.equals("/");
    if (isRoot) {
      String asJson = jsonMapper.writeValueAsString(assetStore.rootDirectory);
      exchange.sendResponse(200, asJson);
      return;
    }

    if (subPath.contains("/")) {
      exchange.sendResponse(
          400, "Invalid subpath: " + subPath + " - mock does not support subdirectories");
      return;
    }

    AssetStore.Secret asset = assetStore.findAssetInRootByTitle(subPath);
    if (asset == null) {
      exchange.sendResponse(404, "Asset not found: " + subPath);
      return;
    }

    String asJson = jsonMapper.writeValueAsString(asset.asAsset());
    exchange.sendResponse(200, asJson);
  }
}

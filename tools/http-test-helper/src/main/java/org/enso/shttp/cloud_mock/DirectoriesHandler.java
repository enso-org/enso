package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.List;
import org.enso.shttp.HttpMethod;

/**
 * Lists assets in a directory.
 *
 * <p>In the mock, only user's home directory can be listed, and it may only contain secrets.
 */
public class DirectoriesHandler implements CloudHandler {

  private static final String DIRECTORIES = "directories";

  private final AssetStore assetStore;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public DirectoriesHandler(AssetStore assetStore) {
    this.assetStore = assetStore;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(DIRECTORIES);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    if (exchange.getMethod() == HttpMethod.GET) {
      listDirectory(exchange.subPath().substring(DIRECTORIES.length() + 1), exchange);
    } else {
      exchange.sendResponse(
          405, "Method not allowed: " + exchange.getMethod() + " - mock only allows GET");
    }
  }

  private void listDirectory(String parentId, CloudExchange exchange) throws IOException {
    final String effectiveParentId = parentId.isEmpty() ? AssetStore.HOME_DIRECTORY_ID : parentId;
    ListDirectoryResponse response =
        new ListDirectoryResponse(
            assetStore.listAssets(effectiveParentId).stream()
                .map(AssetStore.Secret::asAsset)
                .toList());
    String asJson = jsonMapper.writeValueAsString(response);
    exchange.sendResponse(200, asJson);
  }

  private record ListDirectoryResponse(List<AssetStore.Asset> assets) {}
}

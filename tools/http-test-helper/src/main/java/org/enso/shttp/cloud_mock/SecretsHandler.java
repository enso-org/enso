package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import org.enso.shttp.HttpMethod;

public class SecretsHandler implements CloudHandler {
  private static final String SECRETS = "secrets";

  private final AssetStore assetStore;
  private final ObjectMapper jsonMapper = new ObjectMapper();

  public SecretsHandler(AssetStore assetStore) {
    this.assetStore = assetStore;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(SECRETS);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    if (exchange.getMethod() == HttpMethod.POST) {
      createSecret(exchange);
    } else {
      exchange.sendResponse(
          405, "Method not allowed: " + exchange.getMethod() + " - mock only allows POST");
    }
  }

  private void createSecret(CloudExchange exchange) throws IOException {
    JsonNode root = jsonMapper.readTree(exchange.decodeBodyAsText());
    String name = root.get("name").asText();
    String value = root.get("value").asText();
    String parentId =
        root.has("parentDirectoryId")
            ? root.get("parentDirectoryId").asText()
            : AssetStore.HOME_DIRECTORY_ID;

    if (assetStore.exists(parentId, name)) {
      exchange.sendResponse(400, "{\"code\": \"resource_already_exists\"}");
      return;
    }

    String createdSecretId = assetStore.createSecret(parentId, name, value);
    String asJson = jsonMapper.writeValueAsString(createdSecretId);
    exchange.sendResponse(200, asJson);
  }
}

package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.json.JsonMapper;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.enso.shttp.HttpMethod;

public class HiddenSecretsHandler implements CloudHandler {
  private static final String HIDDEN_SECRETS = "s3cr3tz";

  private final AssetStore assetStore;
  private final JsonMapper jsonMapper = new JsonMapper();

  public HiddenSecretsHandler(AssetStore assetStore) {
    this.assetStore = assetStore;
  }

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(HIDDEN_SECRETS);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    if (exchange.getMethod() == HttpMethod.GET) {
      getSecret(exchange.subPath().substring(HIDDEN_SECRETS.length() + 1), exchange);
    } else {
      exchange.sendResponse(404, "Not found: " + exchange.subPath());
    }
  }

  private void getSecret(String id, CloudExchange exchange) throws IOException {
    var secret = assetStore.findSecretById(id);
    if (secret == null) {
      exchange.sendResponse(404, "Secret not found: " + id);
    } else {
      String encoded =
          java.util.Base64.getEncoder()
              .encodeToString(secret.value().getBytes(StandardCharsets.UTF_8));
      String asJson = jsonMapper.writeValueAsString(encoded);
      exchange.sendResponse(200, asJson);
    }
  }
}

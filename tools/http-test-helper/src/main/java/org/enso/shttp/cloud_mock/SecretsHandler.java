package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import org.enso.shttp.HttpMethod;

public class SecretsHandler implements CloudHandler {

  private final String SECRETS = "secrets";
  private final String HIDDEN_SECRETS = "s3cr3tz";

  @Override
  public boolean canHandle(String subPath) {
    return subPath.startsWith(SECRETS) || subPath.startsWith(HIDDEN_SECRETS);
  }

  @Override
  public void handleCloudAPI(CloudExchange exchange) throws IOException {
    HttpMethod method;
    try {
      method = HttpMethod.valueOf(exchange.getHttpExchange().getRequestMethod());
    } catch (IllegalArgumentException e) {
      exchange.sendResponse(
          400, "Invalid method: " + exchange.getHttpExchange().getRequestMethod());
      return;
    }

    if (exchange.subPath().equals(SECRETS)) {
      handleTopLevel(method, exchange);
    } else if (exchange.subPath().startsWith(SECRETS)) {
      handleSecretSpecific(method, exchange.subPath().substring(SECRETS.length() + 1), exchange);
    } else if (exchange.subPath().startsWith(HIDDEN_SECRETS)) {
      if (method == HttpMethod.GET) {
        getSecret(exchange.subPath().substring(HIDDEN_SECRETS.length() + 1), exchange);
      } else {
        exchange.sendResponse(404, "Not found: " + exchange.subPath());
      }
    } else {
      exchange.sendResponse(404, "Not found: " + exchange.subPath());
    }
  }

  private void handleSecretSpecific(HttpMethod method, String name, CloudExchange exchange)
      throws IOException {
    switch (method) {
      case DELETE:
        deleteSecret(name, exchange);
        break;
      default:
        exchange.sendResponse(405, "Method not allowed: " + method);
    }
  }

  private void handleTopLevel(HttpMethod method, CloudExchange exchange) throws IOException {
    switch (method) {
      case GET:
        listSecrets(exchange);
        break;
      case POST:
        createSecret(exchange);
        break;
      default:
        exchange.sendResponse(405, "Method not allowed: " + method);
    }
  }

  private void createSecret(CloudExchange exchange) throws IOException {
    JsonNode root = jsonMapper.readTree(exchange.decodeBodyAsText());
    String name = root.get("name").asText();
    String value = root.get("value").asText();
    String parentId = root.has("parentDirectoryId") ? root.get("parentDirectoryId").asText() : ROOT;
    String secretId = "secret-" + UUID.randomUUID();
    accessRoot(parentId).put(secretId, new Secret(name, value));
    String asJson = jsonMapper.writeValueAsString(secretId);
    exchange.sendResponse(200, asJson);
  }

  private void listSecrets(CloudExchange exchange) throws IOException {
    // TODO currently the cloud API does not seem to handle a parent_id parameter, so we always rely
    // on ROOT
    String parentId = ROOT;
    ListSecretsResponse response =
        new ListSecretsResponse(
            accessRoot(parentId).entrySet().stream()
                .map(
                    entry -> new ListSecretsResponse.Element(entry.getKey(), entry.getValue().name))
                .toList());
    String asJson = jsonMapper.writeValueAsString(response);
    exchange.sendResponse(200, asJson);
  }

  private record ListSecretsResponse(List<Element> secrets) {
    public record Element(String id, String name) {}
  }

  private void getSecret(String id, CloudExchange exchange) throws IOException {
    String parentId = ROOT;
    Secret secret = accessRoot(parentId).get(id);
    if (secret == null) {
      exchange.sendResponse(404, "Secret not found: " + id);
    } else {
      String encoded =
          java.util.Base64.getEncoder()
              .encodeToString(secret.value.getBytes(StandardCharsets.UTF_8));
      exchange.sendResponse(200, '"' + encoded + '"');
    }
  }

  private void deleteSecret(String name, CloudExchange exchange) throws IOException {
    String parentId = ROOT;
    boolean existed = accessRoot(parentId).remove(name) != null;
    if (existed) {
      exchange.sendResponse(200, "");
    } else {
      exchange.sendResponse(404, "Secret not found: " + name);
    }
  }

  private static final String ROOT = "<//root//>";

  private record Secret(String name, String value) {}

  // <root> -> <secret_id> -> <secret_value>
  private HashMap<String, HashMap<String, Secret>> mapping = new HashMap<>();

  private HashMap<String, Secret> accessRoot(String rootId) {
    return mapping.computeIfAbsent(rootId, k -> new HashMap<>());
  }

  private final ObjectMapper jsonMapper = new ObjectMapper();
}

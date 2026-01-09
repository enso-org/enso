package org.enso.base.enso_cloud;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.Map;
import org.enso.base.cache.ReloadDetector;

/** * Internal class to read secrets from the Enso Cloud. */
class EnsoSecretReader implements ReloadDetector.HasClearableCache {
  static final EnsoSecretReader INSTANCE = new EnsoSecretReader();

  private final Map<String, String> secrets = new HashMap<>();

  private EnsoSecretReader() {
    ReloadDetector.register(this);
  }

  void flushCache() {
    secrets.clear();
  }

  void removeFromCache(String secretId) {
    ReloadDetector.clearOnReloadIfRegistered(this);

    secrets.remove(secretId);
  }

  /**
   * * Reads a secret from the Enso Cloud.
   *
   * @param secretId the ID of the secret to read.
   * @return the secret value.
   */
  String readSecret(String secretId) {
    ReloadDetector.clearOnReloadIfRegistered(this);

    if (secrets.containsKey(secretId)) {
      return secrets.get(secretId);
    }

    return fetchSecretValue(secretId, 3);
  }

  private String fetchSecretValue(String secretId, int retryCount) {
    var apiUri = CloudAPI.getInstance().getAPIRootURI() + "s3cr3tz/" + secretId;
    var client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build();
    var request =
        HttpRequest.newBuilder()
            .uri(URI.create(apiUri))
            .header("Authorization", "Bearer " + AuthenticationProvider.INSTANCE.getAccessToken())
            .GET()
            .build();

    HttpResponse<String> response;

    try {
      response = client.send(request, HttpResponse.BodyHandlers.ofString());
    } catch (IOException | InterruptedException e) {
      if (retryCount < 0) {
        throw new IllegalArgumentException("Unable to read secret.");
      } else {
        return fetchSecretValue(secretId, retryCount - 1);
      }
    }

    int status = response.statusCode();
    if (status == 401 || status >= 500) {
      if (retryCount < 0) {
        String kind = status >= 500 ? "server" : "authentication";
        throw new IllegalArgumentException(
            "Unable to read secret - numerous " + kind + " failures (status code " + status + ").");
      } else {
        // We forcibly refresh the access token and try again.
        AuthenticationProvider.INSTANCE.getAuthenticationService().force_refresh();
        return fetchSecretValue(secretId, retryCount - 1);
      }
    }

    if (status != 200) {
      throw new IllegalArgumentException(
          "Unable to read secret - the service responded with status " + status + ".");
    }

    var secretJSON = response.body();
    var secretValue = readValueFromString(secretJSON);
    secrets.put(secretId, secretValue);
    return secretValue;
  }

  private String readValueFromString(String json) {
    var base64 = json.substring(1, json.length() - 1).translateEscapes();
    return new String(
        java.util.Base64.getDecoder().decode(base64), java.nio.charset.StandardCharsets.UTF_8);
  }

  @Override /* HasClearableCache */
  public void clearCache() {
    flushCache();
  }

  /** Visible for testing */
  public int getCacheSize() {
    return secrets.size();
  }
}

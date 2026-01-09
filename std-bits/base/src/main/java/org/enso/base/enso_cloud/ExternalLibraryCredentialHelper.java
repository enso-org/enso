package org.enso.base.enso_cloud;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.util.List;

/**
 * An entry point allowing external libraries to access Enso credentials.
 *
 * <p>It will only allow access from trusted code locations.
 */
public class ExternalLibraryCredentialHelper {
  public record CredentialReference(String secretId, String serviceName) {
    public void expectForService(String expectedServiceName) {
      if (!serviceName.equals(expectedServiceName)) {
        throw new IllegalArgumentException(
            "Expected credentials for " + expectedServiceName + ", but was: " + serviceName + ".");
      }
    }
  }

  public record AccessToken(String token, ZonedDateTime expirationDate) {}

  public record RefreshToken(String token, ZonedDateTime expirationDate, JsonNode metadata) {
    private static RefreshToken parse(JsonNode tokenObject) {
      assert tokenObject.isObject();
      var tokenValue = tokenObject.get("refreshToken");
      var expirationDate = tokenObject.get("expirationDate");
      var metadata = tokenObject.get("metadata");
      if (tokenValue == null
          || !tokenValue.isTextual()
          || (expirationDate != null && !expirationDate.isTextual())
          || (metadata != null && !metadata.isObject())) {
        throw malformedCredential();
      }

      String refreshToken = tokenValue.asText();
      ZonedDateTime expiration = null;
      if (expirationDate != null) {
        try {
          expiration = ZonedDateTime.parse(expirationDate.asText());
        } catch (DateTimeParseException e) {
          throw new IllegalStateException("Failed to parse expiration date in credential payload.");
        }
      }

      return new RefreshToken(refreshToken, expiration, metadata);
    }
  }

  public record CredentialConfig(JsonNode input, RefreshToken refreshToken) {}

  public static CredentialConfig readCredential(CredentialReference credentialReference)
      throws EnsoSecretAccessDenied {
    RestrictedAccess.checkAccess(allowParseCredential);

    String secretPayload = EnsoSecretReader.INSTANCE.readSecret(credentialReference.secretId());
    ObjectMapper jsonMapper = new ObjectMapper();
    JsonNode json;
    try {
      json = jsonMapper.readTree(secretPayload);
    } catch (JsonProcessingException e) {
      throw malformedCredential();
    }

    var tokenField = json.get("token");
    if (tokenField == null) {
      throw new IllegalStateException(
          "The credential is missing token information. Please finish the authentication flow"
              + " before using it.");
    }

    if (!tokenField.isObject()) {
      throw malformedCredential();
    }

    var inputField = json.get("input");
    if (inputField == null || !inputField.isObject()) {
      throw malformedCredential();
    }

    return new CredentialConfig(inputField, RefreshToken.parse(tokenField));
  }

  public static AccessToken requestAccessToken(CredentialReference credentialReference)
      throws EnsoSecretAccessDenied {
    var cloudAPI = CloudAPI.getInstance();
    RestrictedAccess.checkAccess(allowRefreshCredential);
    var apiUri =
        cloudAPI.getAPIRootURI()
            + "oauth/"
            + credentialReference.serviceName().toLowerCase()
            + "/refresh/"
            + credentialReference.secretId();
    var client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS).build();
    var request =
        HttpRequest.newBuilder()
            .uri(URI.create(apiUri))
            .header("Authorization", "Bearer " + AuthenticationProvider.INSTANCE.getAccessToken())
            .POST(HttpRequest.BodyPublishers.noBody())
            .build();
    // TODO retries?
    HttpResponse<String> response;
    try {
      response = client.send(request, HttpResponse.BodyHandlers.ofString());
    } catch (IOException | InterruptedException e) {
      // TODO retries
      throw new IllegalStateException(
          "Failed to refresh the " + credentialReference.serviceName() + " token.");
    }

    int status = response.statusCode();
    if (status != 200) {
      throw new IllegalArgumentException(
          "Unable to refresh the token - the service responded with status " + status + ".");
    }

    ObjectMapper jsonMapper = new ObjectMapper();
    JsonNode json;
    try {
      json = jsonMapper.readTree(response.body());
    } catch (JsonProcessingException e) {
      throw malformedTokenResponse();
    }

    var tokenField = json.get("token");
    if (tokenField == null || !tokenField.isTextual()) {
      throw malformedTokenResponse();
    }

    ZonedDateTime expirationDate = null;
    var expirationField = json.get("expirationDate");
    if (expirationField != null) {
      if (!expirationField.isTextual()) {
        throw malformedTokenResponse();
      }

      try {
        expirationDate = ZonedDateTime.parse(expirationField.asText());
      } catch (DateTimeParseException e) {
        throw new IllegalStateException("Failed to parse expiration date in token response.");
      }
    }

    return new AccessToken(tokenField.asText(), expirationDate);
  }

  private static RuntimeException malformedTokenResponse() {
    throw new IllegalStateException("Malformed token response.");
  }

  public static RuntimeException malformedCredential() {
    // We specifically do not pass the original exception as cause, to avoid leaking any secrets
    // that it could contain.
    throw new IllegalStateException("Unexpected: Malformed credential payload.");
  }

  private static final List<RestrictedAccess.AccessLocation> allowParseCredential =
      List.of(
          new RestrictedAccess.AccessLocation(
              "org.enso.snowflake.SnowflakeCloudCredentials", "unsafeReadCredential"));

  private static final List<RestrictedAccess.AccessLocation> allowRefreshCredential =
      List.of(
          new RestrictedAccess.AccessLocation(
              "org.enso.google.GoogleOAuthHelper$CloudRenewableGoogleCredentials", "refresh"),
          new RestrictedAccess.AccessLocation("org.enso.saas.strava.StravaService", "refresh"),
          new RestrictedAccess.AccessLocation("org.enso.microsoft.ms365.MS365Service", "refresh"));
}

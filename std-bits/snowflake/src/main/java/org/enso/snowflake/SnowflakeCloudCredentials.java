package org.enso.snowflake;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import org.enso.base.enso_cloud.ExternalLibraryCredentialHelper;
import org.enso.base.enso_cloud.HideableValue;
import org.enso.base.net.http.UrlencodedBodyBuilder;

public final class SnowflakeCloudCredentials {
  private static SnowflakeCredentialConfig unsafeReadCredential(
      ExternalLibraryCredentialHelper.CredentialReference credentialReference) {
    credentialReference.expectForService("Snowflake");
    ExternalLibraryCredentialHelper.CredentialConfig config =
        ExternalLibraryCredentialHelper.readCredential(credentialReference);
    var input = parseInputPart(config.input());
    var token = SnowflakeRefreshToken.parse(config.refreshToken());
    return new SnowflakeCredentialConfig(input, token);
  }

  private static CredentialInput parseInputPart(JsonNode inputObject) {
    assert inputObject.isObject();
    var accountField = inputObject.get("account");
    var clientIdField = inputObject.get("clientId");
    var clientSecretField = inputObject.get("clientSecret");
    if (accountField == null
        || !accountField.isTextual()
        || clientIdField == null
        || !clientIdField.isTextual()
        || clientSecretField == null
        || !clientSecretField.isTextual()) {
      throw ExternalLibraryCredentialHelper.malformedCredential();
    }

    return new CredentialInput(
        accountField.asText(), clientIdField.asText(), clientSecretField.asText());
  }

  public static List<HideableValue.KeyValuePair> makePairs(
      ExternalLibraryCredentialHelper.CredentialReference credentialReference) {
    SnowflakeCredentialConfig credentials = unsafeReadCredential(credentialReference);
    AccessToken accessToken = credentials.refresh();
    List<HideableValue.KeyValuePair> secureProperties = new ArrayList<>();
    secureProperties.add(
        new HideableValue.KeyValuePair("authenticator", HideableValue.plain("oauth")));
    secureProperties.add(
        new HideableValue.KeyValuePair("user", HideableValue.plain(accessToken.username())));
    secureProperties.add(
        new HideableValue.KeyValuePair("token", HideableValue.plain(accessToken.token())));
    return secureProperties;
  }

  private static String extractTokenFromResponse(HttpResponse<String> response) {
    ObjectMapper jsonMapper = new ObjectMapper();
    try {
      var json = jsonMapper.readTree(response.body());
      var tokenField = json.get("access_token");
      if (tokenField == null || !tokenField.isTextual()) {
        // This is rethrown with a message by the catch block.
        throw new IllegalStateException();
      }

      return tokenField.asText();
    } catch (Exception e) {
      // We specifically do not pass the original exception as cause, to avoid leaking any secrets
      // that it could contain.
      throw new IllegalStateException("Failed to extract access token from response.");
    }
  }

  private record SnowflakeRefreshToken(
      String token, ZonedDateTime expirationDate, String username) {
    private static SnowflakeRefreshToken parse(ExternalLibraryCredentialHelper.RefreshToken token) {
      if (token.expirationDate() == null) {
        throw ExternalLibraryCredentialHelper.malformedCredential();
      }

      var usernameField = token.metadata().get("username");
      if (usernameField == null || !usernameField.isTextual()) {
        throw ExternalLibraryCredentialHelper.malformedCredential();
      }

      String username = usernameField.asText();
      return new SnowflakeRefreshToken(token.token(), token.expirationDate(), username);
    }

    private boolean isExpired() {
      return expirationDate.isBefore(ZonedDateTime.now());
    }
  }

  private record AccessToken(String token, String username) {}

  private record CredentialInput(String account, String clientId, String clientSecret) {
    private String authorizationHeader() {
      return "Basic "
          + java.util.Base64.getEncoder()
              .encodeToString((clientId + ":" + clientSecret).getBytes(StandardCharsets.UTF_8));
    }

    private URI tokenUri() {
      return URI.create("https://" + account + ".snowflakecomputing.com/oauth/token-request");
    }
  }

  private record SnowflakeCredentialConfig(CredentialInput input, SnowflakeRefreshToken token) {
    private AccessToken refresh() {
      if (token.isExpired()) {
        // TODO other exception type?
        throw new IllegalStateException(
            "The Cloud Credentials have expired and must be renewed. Please go to the Dashboard and"
                + " re-authenticate.");
      }

      try {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest.Builder requestBuilder = HttpRequest.newBuilder();
        var body =
            new UrlencodedBodyBuilder()
                .add_part_text("grant_type", "refresh_token")
                .add_part_text("refresh_token", token.token)
                .build();
        var request =
            requestBuilder
                .POST(body)
                .uri(input.tokenUri())
                .header("Authorization", input.authorizationHeader())
                .header("Content-Type", "application/x-www-form-urlencoded")
                .build();
        // TODO retries?
        var response = client.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
          throw new IllegalStateException(
              "Failed to refresh the Cloud Credentials, service responded with code "
                  + response.statusCode()
                  + ".");
        }

        String accessToken = extractTokenFromResponse(response);
        return new AccessToken(accessToken, token.username);
      } catch (Exception e) {
        // We specifically do not pass the original exception as cause, to avoid leaking any secrets
        // that it could contain.
        throw new IllegalStateException("Failed to refresh the Cloud Credentials.");
      }
    }
  }
}

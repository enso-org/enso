package org.enso.snowflake;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;
import org.enso.base.enso_cloud.ExternalLibrarySecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import org.enso.base.net.http.UrlencodedBodyBuilder;
import org.enso.database.JDBCProxy;
import org.graalvm.collections.Pair;

public final class SnowflakeCloudCredentials {
  private static CredentialConfig unsafeParseCredential(HideableValue credentialReference) {
    String secretPayload = ExternalLibrarySecretHelper.resolveValue(credentialReference);
    ObjectMapper jsonMapper = new ObjectMapper();
    try {
      var json = jsonMapper.readTree(secretPayload);
      var tokenField = json.get("token");
      if (tokenField == null) {
        throw new IllegalStateException(
            "The credential is missing token information. Please finish the authentication flow"
                + " before using it.");
      }

      if (!tokenField.isObject()) {
        throw malformedCredential();
      }

      RefreshToken token = parseTokenPart(tokenField);

      var inputField = json.get("input");
      if (inputField == null || !inputField.isObject()) {
        throw malformedCredential();
      }

      CredentialInput input = parseInputPart(inputField);
      return new CredentialConfig(input, token);
    } catch (Exception e) {
      // We specifically do not pass the original exception as cause, to avoid leaking any secrets
      // that it could contain.
      throw new IllegalStateException(
          "Failed to parse secret payload as credential. Perhaps the secret was not created in the"
              + " Dashboard as a Credential?");
    }
  }

  private static RefreshToken parseTokenPart(JsonNode tokenObject) {
    assert tokenObject.isObject();
    var tokenValue = tokenObject.get("refreshToken");
    var expirationDate = tokenObject.get("expirationDate");
    var metadata = tokenObject.get("metadata");
    if (tokenValue == null
        || !tokenValue.isTextual()
        || expirationDate == null
        || !expirationDate.isTextual()
        || metadata == null
        || !metadata.isObject()) {
      throw malformedCredential();
    }

    String refreshToken = tokenValue.asText();
    ZonedDateTime expiration;
    try {
      expiration = ZonedDateTime.parse(expirationDate.asText());
    } catch (DateTimeParseException e) {
      throw new IllegalStateException("Failed to parse expiration date in credential payload.");
    }

    var usernameField = metadata.get("username");
    if (usernameField == null || !usernameField.isTextual()) {
      throw malformedCredential();
    }

    String username = usernameField.asText();
    return new RefreshToken(refreshToken, expiration, username);
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
      throw malformedCredential();
    }

    return new CredentialInput(
        accountField.asText(), clientIdField.asText(), clientSecretField.asText());
  }

  private static RuntimeException malformedCredential() {
    // We specifically do not pass the original exception as cause, to avoid leaking any secrets
    // that it could contain.
    throw new IllegalStateException("Unexpected: Malformed credential payload.");
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

  public static Connection makeConnection(
      String url, List<Pair<String, HideableValue>> properties, HideableValue credentialReference)
      throws SQLException {
    CredentialConfig credentials = unsafeParseCredential(credentialReference);
    AccessToken accessToken = credentials.refresh();
    var secureProperties = new ArrayList<>(properties);
    secureProperties.add(Pair.create("authenticator", new HideableValue.PlainValue("oauth")));
    secureProperties.add(Pair.create("user", new HideableValue.PlainValue(accessToken.username())));
    secureProperties.add(Pair.create("token", new HideableValue.PlainValue(accessToken.token())));
    return JDBCProxy.getConnection(url, secureProperties);
  }

  private record AccessToken(String token, String username) {}

  private record RefreshToken(String token, ZonedDateTime expirationDate, String username) {
    private boolean isExpired() {
      return expirationDate.isBefore(ZonedDateTime.now());
    }
  }

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

  private record CredentialConfig(CredentialInput input, RefreshToken token) {
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

package org.enso.base.enso_cloud;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import java.util.List;

/**
 * An entry point allowing external libraries to access Enso credentials.
 *
 * <p>It will only allow access from trusted code locations.
 */
public class ExternalLibraryCredentialHelper {
  public record CredentialReference(String secretId) {}

  public record AccessToken(String token) {}

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

    String secretPayload = EnsoSecretReader.readSecret(credentialReference.secretId());
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

      var inputField = json.get("input");
      if (inputField == null || !inputField.isObject()) {
        throw malformedCredential();
      }

      return new CredentialConfig(inputField, RefreshToken.parse(tokenField));
    } catch (Exception e) {
      // We specifically do not pass the original exception as cause, to avoid leaking any secrets
      // that it could contain.
      throw new IllegalStateException(
          "Failed to parse secret payload as credential. Perhaps the secret was not created in the"
              + " Dashboard as a Credential?");
    }
  }

  public static AccessToken requestAccessToken(CredentialReference credentialReference)
      throws EnsoSecretAccessDenied {
    RestrictedAccess.checkAccess(allowRefreshCredential);
    // TODO
    return null;
  }

  public static RuntimeException malformedCredential() {
    // We specifically do not pass the original exception as cause, to avoid leaking any secrets
    // that it could contain.
    throw new IllegalStateException("Unexpected: Malformed credential payload.");
  }

  private static final List<RestrictedAccess.AccessLocation> allowParseCredential =
      List.of(
          new RestrictedAccess.AccessLocation(
              "org.enso.snowflake.SnowflakeCloudCredentials", "unsafeParseCredential"));

  private static final List<RestrictedAccess.AccessLocation> allowRefreshCredential =
      List.of(
          new RestrictedAccess.AccessLocation(
              "org.enso.google.GoogleOAuthSecretReader", "createCredentialFromSecretValue"));
}

package org.enso.logging.service.common;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Inspired from {@code Standard.Base.Enso_Cloud.Internal.Authentication} module. */
public final class TokenRefresher {
  private static final Logger LOGGER = LoggerFactory.getLogger(TokenRefresher.class);
  private static final Map<String, String> HEADERS =
      Map.of(
          "Content-Type", "application/x-amz-json-1.1",
          "X-Amz-Target", "AWSCognitoIdentityProviderService.InitiateAuth");

  /**
   * The amount of time before the token expiration that we pro-actively refresh it to reduce the
   * chance of it expiring during a request.
   */
  private static final Duration TOKEN_EARLY_REFRESH_PERIOD = Duration.ofMinutes(2);

  private final URI refreshUri;
  private final String clientId;
  private final String refreshToken;
  private final HttpClient httpClient;

  public TokenRefresher(URI refreshUri, String clientId, String refreshToken) {
    this.refreshUri = refreshUri;
    this.clientId = clientId;
    this.refreshToken = refreshToken;
    this.httpClient = HttpClient.newBuilder().build();
  }

  /**
   * Fetches the new refreshed access token and blocks until the response is received.
   *
   * @return null if the token could not be refreshed, otherwise the new access token.
   */
  public AuthenticationData fetchNewAccessToken() {
    var reqBldr = HttpRequest.newBuilder();
    reqBldr.uri(refreshUri);
    for (var entry : HEADERS.entrySet()) {
      reqBldr.header(entry.getKey(), entry.getValue());
    }
    var payload = RefreshTokenPayload.createRefreshTokenRequest(refreshToken, clientId);
    reqBldr.POST(BodyPublishers.ofString(payload));
    var req = reqBldr.build();
    LOGGER.trace(
        "Sending request to refresh token: POST uri:{}, headers:{}, body:{}",
        req.uri(),
        req.headers(),
        payload);
    String body;
    try {
      var response = httpClient.send(req, HttpResponse.BodyHandlers.ofString());
      if (response.statusCode() != 200) {
        LOGGER.error("Failed to refresh token: [{}] {}", response.statusCode(), response.body());
        return null;
      }
      body = response.body();
    } catch (IOException | InterruptedException e) {
      LOGGER.error("Failed to refresh token", e);
      return null;
    }
    var resp = RefreshTokenPayload.decodeResponse(body);
    var tokenLifeTime = Duration.ofSeconds(resp.authenticationResult().expiresIn());
    if (tokenLifeTime.compareTo(TOKEN_EARLY_REFRESH_PERIOD) < 0) {
      LOGGER.error(
          "Token lifetime is too short: {}, smaller than our minimum lifetime of {}.",
          tokenLifeTime,
          TOKEN_EARLY_REFRESH_PERIOD);
      return null;
    }
    var responseReceivedTime = Instant.now().atZone(ZoneId.of("UTC"));
    var expireAt = responseReceivedTime.plus(tokenLifeTime);
    var accToken = resp.authenticationResult().accessToken();
    return new AuthenticationData(accToken, expireAt);
  }
}

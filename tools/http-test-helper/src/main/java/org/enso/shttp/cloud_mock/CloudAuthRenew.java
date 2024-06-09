package org.enso.shttp.cloud_mock;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.util.Objects;
import org.enso.shttp.SimpleHttpHandler;

/** An endpoint simulating the refresh token flow. */
public class CloudAuthRenew extends SimpleHttpHandler {
  private int counter = 0;

  @Override
  protected void doHandle(HttpExchange exchange) throws IOException {
    JsonNode root = jsonMapper.readTree(decodeBodyAsText(exchange));
    String flow = root.get("AuthFlow").asText();
    if (!Objects.equals(flow, "REFRESH_TOKEN_AUTH")) {
      sendResponse(400, "Invalid flow: " + flow, exchange);
      return;
    }

    String refreshToken = root.get("AuthParameters").get("REFRESH_TOKEN").asText();
    switch (refreshToken) {
      case "TEST-ENSO-REFRESH-caffee" -> sendRenewedToken(exchange);
      case "GET-MALFORMED-RESPONSE" -> sendMalformedResponse(exchange);
      case null, default -> sendResponse(401, "Invalid refresh token.", exchange);
    }
  }

  private void sendRenewedToken(HttpExchange exchange) throws IOException {
    String newToken = "TEST-RENEWED-" + (counter++);
    var response = new RenewResponse(new AuthenticationResult(newToken, "Bearer", 3600));
    sendResponse(200, jsonMapper.writeValueAsString(response), exchange);
  }

  private void sendMalformedResponse(HttpExchange exchange) throws IOException {
    var response = new MalformedRenewResponse(new MalformedAuthenticationResult("foobar", 123, 3600));
    sendResponse(200, jsonMapper.writeValueAsString(response), exchange);
  }

  private final ObjectMapper jsonMapper = new ObjectMapper();

  {
    jsonMapper.setPropertyNamingStrategy(PropertyNamingStrategies.UPPER_CAMEL_CASE);
  }

  private record RenewResponse(AuthenticationResult authenticationResult) {}

  private record AuthenticationResult(String accessToken, String tokenType, int expiresIn) {}

  private record MalformedRenewResponse(MalformedAuthenticationResult authenticationResult) {}

  private record MalformedAuthenticationResult(String accessToken, int tokenType, int expiresIn) {}
}

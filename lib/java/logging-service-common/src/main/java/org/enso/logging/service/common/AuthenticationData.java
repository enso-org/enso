package org.enso.logging.service.common;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;

public record AuthenticationData(String accessToken, ZonedDateTime expireAt) {
  public static AuthenticationData fromCredentials(Credentials credentials) {
    ZonedDateTime expireAt;
    try {
      expireAt = Instant.parse(credentials.expireAt()).atZone(ZoneId.of("UTC"));
    } catch (DateTimeParseException e) {
      throw new AssertionError("Should not reach here", e);
    }
    return new AuthenticationData(credentials.accessToken(), expireAt);
  }
}

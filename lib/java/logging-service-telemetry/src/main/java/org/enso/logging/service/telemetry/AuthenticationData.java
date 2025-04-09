package org.enso.logging.service.telemetry;

import java.time.LocalDateTime;

public record AuthenticationData(String accessToken, LocalDateTime expireAt) {}

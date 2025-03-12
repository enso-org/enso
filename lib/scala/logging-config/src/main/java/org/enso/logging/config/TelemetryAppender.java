package org.enso.logging.config;

import com.typesafe.config.Config;
import java.net.URI;
import java.nio.file.Path;
import java.time.LocalDateTime;
import org.slf4j.event.Level;

public final class TelemetryAppender extends Appender {
  public static final String appenderName = "telemetry";

  public static Appender parse(Config config) throws MissingConfigurationField {
    return new TelemetryAppender();
  }

  @Override
  public String getName() {
    return appenderName;
  }

  @Override
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return loggerSetup.setupTelemetryAppender();
  }

  private Path credentialsFile() {
    var home = Path.of(System.getProperty("user.home"));
    var credentials = home.resolve(".enso").resolve("credentials");
    if (!credentials.toFile().exists()) {
      throw new IllegalStateException("User not logged in");
    }
    return credentials;
  }

  private static Credentials readCredentials() {
    // TODO: Parse JSON from credentials
    throw new UnsupportedOperationException("unimplemented");
  }

  private record Credentials(String accessToken, LocalDateTime expireAt) {}
}

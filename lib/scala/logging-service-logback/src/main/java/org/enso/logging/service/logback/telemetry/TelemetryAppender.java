package org.enso.logging.service.logback.telemetry;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;
import com.fasterxml.jackson.core.exc.StreamReadException;
import com.fasterxml.jackson.databind.DatabindException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.Arrays;
import com.fasterxml.jackson.annotation.JsonProperty;

// TODO: Presunout sem LogJob, apod.

public final class TelemetryAppender extends AppenderBase<ILoggingEvent> {
  private static final String CREDENTIALS_FILE_ENV = "ENSO_CLOUD_CREDENTIALS_FILE";

  private TelemetryAppender() {}

  public static TelemetryAppender create() {
    // TODO: Read URL endpoint from env vars
    // TODO: No virtual thread executor
    var credentialsFile = credentialsFile();
    if (!credentialsFile.toFile().exists()) {
      return null;
    }
    var credentials = parseCredentials(credentialsFile);
    if (credentials == null) {
      return null;
    }
    return new TelemetryAppender();
  }

  @Override
  protected void append(ILoggingEvent eventObject) {
    var mdcMap = eventObject.getMDCPropertyMap();
    System.out.printf(
        "TelemetryAppender.append: msg='%s', args=%s, loggerName='%s', threadName='%s', mdcMap={%s}"
            + " %n",
        eventObject.getMessage(),
        Arrays.toString(eventObject.getArgumentArray()),
        eventObject.getLoggerName(),
        eventObject.getThreadName(),
        mdcMap);
  }

  private static Path credentialsFile() {
    var env = System.getenv(CREDENTIALS_FILE_ENV);
    if (env != null) {
      return Path.of(env);
    }
    var home = Path.of(System.getProperty("user.home"));
    var credentials = home.resolve(".enso").resolve("credentials");
    return credentials;
  }

  private static Credentials parseCredentials(Path file) {
    assert file.toFile().exists();
    var objectMapper = new ObjectMapper();
    try {
      return objectMapper.readValue(file.toFile(), Credentials.class);
    } catch (IOException e) {
      return null;
    }
  }

  /**
   * The credentials file is created by the IDE once user logs in.
   * We are just reading it.
   */
  private record Credentials(
      @JsonProperty("client_id")
      String clientId,
      @JsonProperty("access_token")
      String accessToken,
      @JsonProperty("refresh_token")
      String refreshToken,
      @JsonProperty("refresh_url")
      String refreshUrl,
      @JsonProperty("expire_at")
      String expireAt) {}
}

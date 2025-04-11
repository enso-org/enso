package org.enso.logging.service.telemetry;

import ch.qos.logback.classic.spi.ILoggingEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import org.enso.logging.service.logback.TelemetryAppender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Background job processing inspired by {@code org.enso.base.enso_cloud.logging.LogApiAccess}.
 * Singleton. See {@link LogFormatter} for the expected format of log messages to this appender.
 *
 * <p>This appender is supposed to be started by the project manager, within {@link
 * org.enso.logging.service.logback.LoggingServer}. The logging events that are received in this
 * {@link TelemetryAppenderImpl#append(ILoggingEvent)} method are received from a socket and
 * deserialized by the logback framework. Thus, the {@link ILoggingEvent#getArgumentArray() log
 * event arguments} are most likely strings.
 */
@org.openide.util.lookup.ServiceProvider(service = TelemetryAppender.class)
public final class TelemetryAppenderImpl extends TelemetryAppender {
  private static final String CREDENTIALS_FILE_ENV = "ENSO_CLOUD_CREDENTIALS_FILE";
  private static final Logger LOGGER =
      LoggerFactory.getLogger(TelemetryAppenderImpl.class.getName());
  private Credentials credentials;
  private boolean credentialsParseFailure;
  private LogJobsProcessor logJobsProcessor;

  @Override
  protected void append(ILoggingEvent eventObject) {
    enqueueJob(eventObject);
  }

  private static Credentials readCredentials() {
    var credentialsFile = credentialsFile();
    if (!credentialsFile.toFile().exists()) {
      LOGGER.warn("Credentials file not found at '{}'. Will not send telemetry", credentialsFile);
      return null;
    }
    Credentials credentials;
    try {
      credentials = parseCredentials(credentialsFile);
    } catch (IOException e) {
      LOGGER.warn(
          "Failed to parse credentials from '{}'. Will not send telemetry", credentialsFile);
      return null;
    }
    LOGGER.debug("Credentials read from '{}': {}", credentialsFile, credentials);
    return credentials;
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

  private static Credentials parseCredentials(Path file) throws IOException {
    assert file.toFile().exists();
    return Credentials$.MODULE$.parseFromFile(file.toFile());
  }

  private void enqueueJob(ILoggingEvent logEvent) {
    if (credentialsParseFailure) {
      return;
    }
    if (credentials == null) {
      credentials = readCredentials();
      if (credentials == null) {
        // If credentials cannot be read, we cannot send anything - bailout.
        LOGGER.error("Credentials cannot be read, stopping the telemetry appender service");
        credentialsParseFailure = true;
        return;
      }
    }
    assert credentials != null;
    if (logJobsProcessor == null) {
      URI refreshUri;
      try {
        refreshUri = new URI(credentials.refreshUrl());
      } catch (URISyntaxException e) {
        LOGGER.error(
            "Failed to parse refresh URL '{}'. Stopping the telemetry appender service",
            credentials.refreshUrl());
        credentialsParseFailure = true;
        return;
      }
      var tokenRefresher =
          new TokenRefresher(refreshUri, credentials.clientId(), credentials.refreshToken());
      var authenticationData = AuthenticationData.fromCredentials(credentials);
      logJobsProcessor =
          new LogJobsProcessor(
              backgroundThreadService, endpoint, authenticationData, tokenRefresher);
    }
    var logMessage = logEventToMessage(logEvent);
    var logJob = new LogJob(logMessage, null);
    logJobsProcessor.enqueueMessage(logJob);
  }

  private static LogMessage logEventToMessage(ILoggingEvent logEvent) {
    return new LogMessage(
        logEvent.getLoggerName(), logEvent.getMessage(), logEvent.getArgumentArray());
  }
}

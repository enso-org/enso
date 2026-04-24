package org.enso.logging.service.common;

import ch.qos.logback.classic.spi.ILoggingEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.concurrent.ThreadPoolExecutor;
import org.slf4j.Logger;

/**
 * Background job processing inspired by {@code org.enso.base.enso_cloud.logging.LogApiAccess}.
 *
 * <p>This base class appender encapsulates the core logic needed to send logs a remote endpoint
 * provided by Enso infrastructure. It is supposed to be started by the project manager, within
 * {@link org.enso.logging.service.logback.LoggingServer}. The logging events that are received in
 * this {@link RemoteAppender#append(ILoggingEvent)} method are received from a socket and
 * deserialized by the logback framework. Thus, the {@link ILoggingEvent#getArgumentArray() log
 * event arguments} are most likely strings.
 */
public abstract class RemoteAppender
    extends org.enso.logging.service.logback.AbstractRemoteAppender {
  private static final String CREDENTIALS_FILE_ENV = "ENSO_CLOUD_CREDENTIALS_FILE";
  private final Logger logger;
  private Credentials credentials;
  private boolean credentialsParseFailure;
  private LogJobsProcessor logJobsProcessor;

  protected abstract String kind();

  public RemoteAppender(Logger logger) {
    this.logger = logger;
  }

  @Override
  protected void append(ILoggingEvent eventObject) {
    enqueueJob(eventObject);
  }

  protected abstract LogJobsProcessor newLogJobsProcessor(
      ThreadPoolExecutor executor,
      URI endpoint,
      AuthenticationData authenticationData,
      TokenRefresher tokenRefresher,
      boolean logConnectionFailures);

  private Credentials readCredentials() {
    var credentialsFile = credentialsFile();
    if (!credentialsFile.toFile().exists()) {
      logger.warn("Credentials file not found at '{}'. Will not send " + kind(), credentialsFile);
      return null;
    }
    Credentials credentials;
    try {
      credentials = parseCredentials(credentialsFile);
    } catch (IOException e) {
      logger.warn(
          "Failed to parse credentials from '{}'. Will not send " + kind(), credentialsFile);
      return null;
    }
    logger.debug("Read credentials from '{}'", credentialsFile);
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
        logger.error("Credentials cannot be read, stopping the " + kind() + " appender service");
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
        logger.error(
            "Failed to parse refresh URL '{}'. Stopping the " + kind() + " appender service",
            credentials.refreshUrl());
        credentialsParseFailure = true;
        return;
      }
      var tokenRefresher =
          new TokenRefresher(refreshUri, credentials.clientId(), credentials.refreshToken());
      var authenticationData = AuthenticationData.fromCredentials(credentials);
      logJobsProcessor =
          newLogJobsProcessor(
              backgroundThreadService,
              endpoint,
              authenticationData,
              tokenRefresher,
              logConnectionFailures);
    }
    var logMessage = logEventToMessage(logEvent);
    var logJob = new LogJob(logMessage, null);
    logJobsProcessor.enqueueMessage(logJob);
  }

  private static LogMessage logEventToMessage(ILoggingEvent logEvent) {
    return new LogMessage(
        logEvent.getLoggerName(),
        logEvent.getMessage(),
        logEvent.getArgumentArray(),
        logEvent.getLevel().levelStr,
        logEvent.getMDCPropertyMap());
  }
}

package org.enso.logging.service.telemetry;

import java.net.URI;
import java.util.concurrent.ThreadPoolExecutor;
import org.enso.logging.service.remote.AuthenticationData;
import org.enso.logging.service.remote.LogJobsProcessor;
import org.enso.logging.service.remote.TokenRefresher;
import org.enso.logging.service.remote.RemoteAppender;
import org.slf4j.LoggerFactory;

/**
 * Background job processing inspired by {@code org.enso.base.enso_cloud.logging.LogApiAccess}.
 * Singleton. See {@link TelemetryLogFormatter} for the expected format of log messages to this
 * appender.
 */
public final class TelemetryAppenderImpl extends RemoteAppender {

  public TelemetryAppenderImpl() {
    super(LoggerFactory.getLogger(TelemetryAppenderImpl.class));
  }

  @Override
  protected String kind() {
    return "telemetry";
  }

  @Override
  protected LogJobsProcessor newLogJobsProcessor(
      ThreadPoolExecutor executor,
      URI endpoint,
      AuthenticationData authenticationData,
      TokenRefresher tokenRefresher,
      boolean logConnectionFailures) {
    return new TelemetryLogJobsProcessor(
        executor, endpoint, authenticationData, tokenRefresher, logConnectionFailures);
  }

  @Override
  public boolean canLogTelemetry() {
    return true;
  }

  @Override
  public boolean canLogGenericMessages() {
    return false;
  }
}

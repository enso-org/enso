package org.enso.logging.service.telemetry;

import java.net.URI;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * Background job processing inspired by {@code org.enso.base.enso_cloud.logging.LogApiAccess}.
 * Singleton. See {@link TelemetryLogFormatter} for the expected format of log messages to this
 * appender.
 */
@org.openide.util.lookup.ServiceProvider(
    service = org.enso.logging.service.logback.RemoteAppender.class)
public final class TelemetryAppenderImpl extends RemoteAppender {
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
}

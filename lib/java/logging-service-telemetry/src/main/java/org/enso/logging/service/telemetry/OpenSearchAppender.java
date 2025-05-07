package org.enso.logging.service.telemetry;

import java.net.URI;
import java.util.concurrent.ThreadPoolExecutor;

@org.openide.util.lookup.ServiceProvider(
    service = org.enso.logging.service.logback.RemoteAppender.class)
public final class OpenSearchAppender extends RemoteAppender {

  @Override
  protected String kind() {
    return "engine";
  }

  @Override
  protected LogJobsProcessor newLogJobsProcessor(
      ThreadPoolExecutor executor,
      URI endpoint,
      AuthenticationData authenticationData,
      TokenRefresher tokenRefresher,
      boolean logConnectionFailures) {
    return new OpenSearchLogJobsProcessor(
        executor, endpoint, authenticationData, tokenRefresher, logConnectionFailures);
  }
}

package org.enso.logging.service.opensearch;

import java.net.URI;
import java.util.concurrent.ThreadPoolExecutor;
import org.enso.logging.service.common.AuthenticationData;
import org.enso.logging.service.common.LogJobsProcessor;
import org.enso.logging.service.common.RemoteAppender;
import org.enso.logging.service.common.TokenRefresher;
import org.slf4j.LoggerFactory;

public final class OpenSearchAppender extends RemoteAppender {

  public OpenSearchAppender() {
    super(LoggerFactory.getLogger(OpenSearchAppender.class));
  }

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

  @Override
  public boolean canLogTelemetry() {
    return false;
  }

  @Override
  public boolean canLogGenericMessages() {
    return true;
  }
}

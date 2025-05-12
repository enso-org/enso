package org.enso.logging.service.opensearch;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadPoolExecutor;
import org.enso.logging.service.ApiMessage;
import org.enso.logging.service.AuthenticationData;
import org.enso.logging.service.LogJob;
import org.enso.logging.service.LogJobsProcessor;
import org.enso.logging.service.TokenRefresher;
import org.slf4j.LoggerFactory;

public final class OpenSearchLogJobsProcessor extends LogJobsProcessor {

  public OpenSearchLogJobsProcessor(
      ThreadPoolExecutor executor,
      URI endpoint,
      AuthenticationData authenticationData,
      TokenRefresher tokenRefresher,
      boolean logConnectionFailures) {
    super(
        executor,
        endpoint,
        authenticationData,
        tokenRefresher,
        logConnectionFailures,
        LoggerFactory.getLogger(OpenSearchLogJobsProcessor.class));
  }

  @Override
  protected String buildPayload(List<LogJob> logJobs) {
    var logs = new ArrayList<ApiMessage.Log>();
    for (var logJob : logJobs) {
      var payloadForLogEvent = OpenSearchLogFormatter.transform(logJob.message());
      if (payloadForLogEvent != null) {
        logs.add(payloadForLogEvent);
      }
    }
    if (logs.size() != logJobs.size()) {
      logger.warn("Failed to build payload for some log events");
    }
    if (logs.isEmpty()) {
      return null;
    } else {
      var payload = ApiMessage.createPayload(logs);
      return ApiMessage.serializePayload(payload);
    }
  }
}

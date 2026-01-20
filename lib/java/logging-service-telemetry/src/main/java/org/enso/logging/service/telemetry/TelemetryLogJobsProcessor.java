package org.enso.logging.service.telemetry;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadPoolExecutor;
import org.enso.logging.service.common.ApiMessage;
import org.enso.logging.service.common.AuthenticationData;
import org.enso.logging.service.common.LogJob;
import org.enso.logging.service.common.LogJobsProcessor;
import org.enso.logging.service.common.TokenRefresher;
import org.slf4j.LoggerFactory;

public final class TelemetryLogJobsProcessor extends LogJobsProcessor {

  public TelemetryLogJobsProcessor(
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
        LoggerFactory.getLogger(TelemetryLogJobsProcessor.class));
  }

  /**
   * Transforms the given log events into JSON payloads.
   *
   * @return null if none of the log events could be transformed into a payload.
   */
  @Override
  protected String buildPayload(List<LogJob> logJobs) {
    var logs = new ArrayList<ApiMessage.Log>();
    for (var logJob : logJobs) {
      var payloadForLogEvent = TelemetryLogFormatter.transform(logJob.message());
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

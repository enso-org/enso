package org.enso.logging.service.telemetry;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.regex.Matcher;
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
      var payloadForLogEvent = transformMessage(logJob.message());
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

  private ApiMessage.Log transformMessage(LogMessage msg) {
    String transformedMsg = msg.message();
    Map<String, Object> metadata;
    if (msg.arguments() == null) {
      metadata = constructMetadata(new Object[] {}, msg.loggerName(), msg.logLevel());
    } else {
      var args = msg.arguments();
      var i = 0;
      var done = false;
      while (i < args.length && !done) {
        var arg = args[i];
        var idx = transformedMsg.indexOf("{}");
        if (idx == -1) {
          done = true;
        } else {
          // Ignore's the index of the argument if one makes a mistake of using it
          transformedMsg =
              transformedMsg.replaceFirst("\\{\\}", Matcher.quoteReplacement(arg.toString()));
          i++;
        }
      }
      var remainingArgs = i < args.length ? Arrays.copyOf(args, i) : new Object[] {};
      metadata = constructMetadata(remainingArgs, msg.loggerName(), msg.logLevel());
    }
    return ApiMessage.createEngineLog(transformedMsg, metadata);
  }

  private Map<String, Object> constructMetadata(
      Object[] remainingArgs, String loggerName, String logLevel) {
    var meta = new HashMap<String, Object>();
    meta.put("loggerName", loggerName);
    meta.put("logLevel", logLevel);
    for (int i = 0; i < remainingArgs.length; i++) {
      meta.put("extra-arg-" + i, remainingArgs[i]);
    }
    return meta;
  }
}

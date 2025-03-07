package org.enso.base.enso_cloud.telemetry;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.enso.base.enso_cloud.logging.LogApiAccess;

public final class TelemetryLog {
  private TelemetryLog() {}

  public static void logAsync(String loggerName, String message, ObjectNode metadata) {
    var event = TelemetryLogMessage.create(loggerName, message, metadata);
    LogApiAccess.INSTANCE.logWithoutConfirmation(event);
  }

  public static final class TelemetryLogError extends RuntimeException {
    public TelemetryLogError(String message, Throwable cause) {
      super(message, cause);
    }
  }
}

package org.enso.base.enso_cloud.telemetry;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.enso_cloud.logging.LogMessage;

public final class TelemetryLogMessage extends LogMessage {

  private TelemetryLogMessage(String message, ObjectNode extraMeta) {
    super(message, extraMeta);
  }

  public static TelemetryLogMessage create(String loggerName, String message, ObjectNode metadata) {
    Objects.requireNonNull(loggerName);
    Objects.requireNonNull(message);
    Objects.requireNonNull(metadata);
    var copy = metadata.deepCopy();
    copy.set("loggerName", TextNode.valueOf(loggerName));
    return new TelemetryLogMessage(message, copy);
  }

  @Override
  protected String kind() {
    return "Telemetry";
  }
}

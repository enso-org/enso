package org.enso.base.enso_cloud.telemetry;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.enso_cloud.logging.LogMessage;

final class TelemetryLogMessage extends LogMessage {

  private final String loggerName;
  private final ObjectNode extraMeta;

  private TelemetryLogMessage(String message, String loggerName, ObjectNode extraMeta) {
    super(message);
    this.loggerName = loggerName;
    this.extraMeta = extraMeta;
  }

  public static TelemetryLogMessage create(String loggerName, String message, ObjectNode metadata) {
    Objects.requireNonNull(loggerName);
    Objects.requireNonNull(message);
    Objects.requireNonNull(metadata);
    return new TelemetryLogMessage(message, loggerName, metadata);
  }

  @Override
  protected String kind() {
    return "Telemetry";
  }

  @Override
  protected ObjectNode extraPayload() {
    return null;
  }

  @Override
  protected ObjectNode extraMetadata() {
    var meta = new ObjectNode(JsonNodeFactory.instance);
    meta.set("loggerName", TextNode.valueOf(loggerName));
    extraMeta.fields().forEachRemaining(entry -> meta.set(entry.getKey(), entry.getValue()));
    return meta;
  }
}

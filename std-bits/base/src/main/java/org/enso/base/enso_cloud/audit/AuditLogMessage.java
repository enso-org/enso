package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.enso_cloud.logging.LogMessage;

final class AuditLogMessage extends LogMessage {

  private static final String OPERATION = "operation";

  private AuditLogMessage(String message, ObjectNode metadata) {
    super(message, metadata);
  }

  public static AuditLogMessage create(String operation, String message, ObjectNode metadata) {
    Objects.requireNonNull(operation);
    Objects.requireNonNull(message);
    Objects.requireNonNull(metadata);
    var copy = metadata.deepCopy();
    copy.set(OPERATION, TextNode.valueOf(operation));
    return new AuditLogMessage(message, copy);
  }

  @Override
  protected String kind() {
    return "Lib";
  }
}

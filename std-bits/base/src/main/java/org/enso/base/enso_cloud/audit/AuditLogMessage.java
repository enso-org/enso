package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

public class AuditLogMessage implements AuditLogAPI.LogMessage {

  /**
   * A reserved field that is currently added by the cloud backend. Duplicating it will lead to internal server errors and log messages being discarded.
   */
  private final static String RESERVED_TYPE = "type";
  private final static String OPERATION = "operation";
  private final static String PROJECT_NAME = "projectName";
  private final static String LOCAL_TIMESTAMP = "localTimestamp";

  private final String projectId;
  private final String projectName;
  private final ZonedDateTime localTimestamp;
  private final String operation;
  private final String message;
  private final ObjectNode metadata;

  public AuditLogMessage(String operation, String message, ObjectNode metadata) {
    this.operation = Objects.requireNonNull(operation);
    this.message = Objects.requireNonNull(message);
    this.metadata = Objects.requireNonNull(metadata);
    checkNoRestrictedField(metadata, RESERVED_TYPE);
    checkNoRestrictedField(metadata, OPERATION);
    checkNoRestrictedField(metadata, PROJECT_NAME);
    checkNoRestrictedField(metadata, LOCAL_TIMESTAMP);
    // TODO
    this.projectId = null;
    this.projectName = "TODO";
    this.localTimestamp = ZonedDateTime.now();
  }

  private static void checkNoRestrictedField(ObjectNode metadata, String fieldName) {
    if (metadata.has(fieldName)) {
      throw new IllegalArgumentException("Metadata cannot contain a field named '" + fieldName + "'");
    }
  }

  private ObjectNode computedMetadata() {
    var copy = metadata.deepCopy();
    copy.set(OPERATION, TextNode.valueOf(operation));
    copy.set(PROJECT_NAME, TextNode.valueOf(projectName));
    copy.set(LOCAL_TIMESTAMP, TextNode.valueOf(localTimestamp.format(DateTimeFormatter.ISO_DATE_TIME)));
    return copy;
  }

  @Override
  public String payload() {
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set("message", TextNode.valueOf(message));
    payload.set("projectId", projectId == null ? NullNode.getInstance() : TextNode.valueOf(projectId));
    payload.set("metadata", computedMetadata());
    payload.set("kind", TextNode.valueOf("Lib"));
    return payload.toString();
  }
}

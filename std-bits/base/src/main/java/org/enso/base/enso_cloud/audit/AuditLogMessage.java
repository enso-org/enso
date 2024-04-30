package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

public class AuditLogMessage implements AuditLogAPI.LogMessage {
  private final static String TYPE = "type";
  private final static String PROJECT_NAME = "project_name";
  private final static String LOCAL_TIMESTAMP = "local_timestamp";

  private final String projectId;
  private final String projectName;
  private final ZonedDateTime localTimestamp;
  private final String type;
  private final String message;
  private final ObjectNode metadata;

  public AuditLogMessage(String type, String message, ObjectNode metadata) {
    this.type = Objects.requireNonNull(type);
    this.message = Objects.requireNonNull(message);
    this.metadata = Objects.requireNonNull(metadata);
    checkNoRestrictedField(metadata, TYPE);
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
    copy.set(TYPE, TextNode.valueOf(type));
    copy.set(PROJECT_NAME, TextNode.valueOf(projectName));
    copy.set(LOCAL_TIMESTAMP, TextNode.valueOf(localTimestamp.format(DateTimeFormatter.ISO_DATE_TIME)));
    return copy;
  }

  @Override
  public String payload() {
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set("message", TextNode.valueOf(message));
    payload.set("project_id", projectId == null ? NullNode.getInstance() : TextNode.valueOf(projectId));
    payload.set("metadata", computedMetadata());
    payload.set("kind", TextNode.valueOf("Lib"));
    return payload.toString();
  }
}

package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.enso_cloud.CloudAPI;
import org.enso.base.enso_cloud.logging.LogMessage;

final class AuditLogMessage extends LogMessage {

  private static final String OPERATION = "operation";
  private static final String PROJECT_ID = "projectId";
  private final String operation;
  private final ObjectNode metadata;
  private final String projectId;

  private AuditLogMessage(String message, String operation, ObjectNode metadata, String projectId) {
    super(message);
    this.operation = operation;
    this.metadata = metadata;
    this.projectId = projectId;
  }

  public static AuditLogMessage create(String operation, String message, ObjectNode metadata) {
    Objects.requireNonNull(operation);
    Objects.requireNonNull(message);
    Objects.requireNonNull(metadata);
    checkNoRestrictedFields(metadata);
    var projectId = CloudAPI.getCloudProjectId();
    return new AuditLogMessage(message, operation, metadata, projectId);
  }

  @Override
  protected String kind() {
    return "Lib";
  }

  @Override
  protected ObjectNode extraPayload() {
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set(
        PROJECT_ID, projectId == null ? NullNode.getInstance() : TextNode.valueOf(projectId));
    return payload;
  }

  @Override
  protected ObjectNode extraMetadata() {
    var meta = new ObjectNode(JsonNodeFactory.instance);
    meta.set(OPERATION, TextNode.valueOf(operation));
    metadata
        .fields()
        .forEachRemaining(
            entry -> {
              meta.set(entry.getKey(), entry.getValue());
            });
    return meta;
  }
}

package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.CurrentEnsoProject;
import org.enso.base.enso_cloud.CloudAPI;

class AuditLogMessage implements AuditLogApiAccess.LogMessage {

  /**
   * A reserved field that is currently added by the cloud backend. Duplicating it will lead to
   * internal server errors and log messages being discarded.
   */
  private static final String RESERVED_TYPE = "type";

  private static final String OPERATION = "operation";
  private static final String PROJECT_NAME = "projectName";
  private static final String PROJECT_ID = "projectId";
  private static final String PROJECT_SESSION_ID = "projectSessionId";
  private static final String LOCAL_TIMESTAMP = "localTimestamp";

  private final String projectId;
  private final String projectName;
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
    checkNoRestrictedField(metadata, PROJECT_SESSION_ID);
    checkNoRestrictedField(metadata, LOCAL_TIMESTAMP);

    this.projectId = CloudAPI.getCloudProjectId();

    var currentProject = CurrentEnsoProject.get();
    this.projectName = currentProject == null ? null : currentProject.fullName();
  }

  private static void checkNoRestrictedField(ObjectNode metadata, String fieldName) {
    if (metadata.has(fieldName)) {
      throw new IllegalArgumentException(
          "Metadata cannot contain a field named '" + fieldName + "'");
    }
  }

  private ObjectNode computedMetadata() {
    var copy = metadata.deepCopy();
    copy.set(OPERATION, TextNode.valueOf(operation));

    // The project name may be null if a script is run outside a project.
    if (projectName != null) {
      copy.set(PROJECT_NAME, TextNode.valueOf(projectName));
    }

    String projectSessionId = CloudAPI.getCloudSessionId();
    if (projectSessionId != null) {
      copy.set(PROJECT_SESSION_ID, TextNode.valueOf(projectSessionId));
    }

    return copy;
  }

  @Override
  public String payload() {
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set("message", TextNode.valueOf(message));
    payload.set(
        PROJECT_ID, projectId == null ? NullNode.getInstance() : TextNode.valueOf(projectId));
    payload.set("metadata", computedMetadata());
    payload.set("kind", TextNode.valueOf("Lib"));
    return payload.toString();
  }
}

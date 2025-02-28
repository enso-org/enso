package org.enso.base.enso_cloud.logging;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.CurrentEnsoProject;
import org.enso.base.enso_cloud.CloudAPI;

/** Base class for log messages that are passed to the OpenSearch cloud endpoint. */
public abstract class LogMessage {
  /**
   * A reserved field that is currently added by the cloud backend. Duplicating it will lead to
   * internal server errors and log messages being discarded.
   */
  private static final String RESERVED_TYPE = "type";

  private static final String PROJECT_NAME = "projectName";
  private static final String PROJECT_ID = "projectId";
  private static final String PROJECT_SESSION_ID = "projectSessionId";
  private static final String LOCAL_TIMESTAMP = "localTimestamp";

  private final String message;
  private final ObjectNode extraMetadata;
  private final String projectId;
  private final String projectName;

  /**
   * @param message
   * @param extraMetadata Optional additional metadata to include in the log message. May be null
   */
  protected LogMessage(String message, ObjectNode extraMetadata) {
    this.message = Objects.requireNonNull(message);
    this.extraMetadata = extraMetadata;
    this.projectId = CloudAPI.getCloudProjectId();
    var currentProject = CurrentEnsoProject.get();
    this.projectName = currentProject == null ? null : currentProject.fullName();
    if (extraMetadata != null) {
      checkNoRestrictedFields(extraMetadata);
    }
  }

  private static void checkNoRestrictedField(ObjectNode metadata, String fieldName) {
    if (metadata.has(fieldName)) {
      throw new IllegalArgumentException(
          "Metadata cannot contain a field named '" + fieldName + "'. Metadata was: " + metadata);
    }
  }

  private static void checkNoRestrictedFields(ObjectNode metadata) {
    checkNoRestrictedField(metadata, RESERVED_TYPE);
    checkNoRestrictedField(metadata, LOCAL_TIMESTAMP);
    checkNoRestrictedField(metadata, PROJECT_NAME);
    checkNoRestrictedField(metadata, PROJECT_ID);
  }

  private ObjectNode computedMetadata() {
    var copy = new ObjectNode(JsonNodeFactory.instance);

    // The project name may be null if a script is run outside a project.
    if (projectName != null) {
      copy.set(PROJECT_NAME, TextNode.valueOf(projectName));
    }

    String projectSessionId = CloudAPI.getCloudSessionId();
    if (projectSessionId != null) {
      copy.set(PROJECT_SESSION_ID, TextNode.valueOf(projectSessionId));
    }

    if (extraMetadata != null) {
      extraMetadata
          .fields()
          .forEachRemaining(
              entry -> {
                copy.set(entry.getKey(), entry.getValue());
              });
    }

    return copy;
  }

  public String payload() {
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set("message", TextNode.valueOf(message));
    payload.set(
        PROJECT_ID, projectId == null ? NullNode.getInstance() : TextNode.valueOf(projectId));
    if (projectName != null) {
      payload.set(PROJECT_NAME, TextNode.valueOf(projectName));
    }
    String projectSessionId = CloudAPI.getCloudSessionId();
    if (projectSessionId != null) {
      payload.set(PROJECT_SESSION_ID, TextNode.valueOf(projectSessionId));
    }
    payload.set("metadata", computedMetadata());
    payload.set("kind", TextNode.valueOf(kind()));
    return payload.toString();
  }

  protected abstract String kind();

  @Override
  public String toString() {
    return payload();
  }
}

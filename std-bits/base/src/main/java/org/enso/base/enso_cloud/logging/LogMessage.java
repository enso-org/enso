package org.enso.base.enso_cloud.logging;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import java.util.Objects;
import org.enso.base.CurrentEnsoProject;
import org.enso.base.enso_cloud.CloudAPI;

/** Base class for log messages that are passed to the cloud endpoint. */
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
  private final String projectId;
  private final String projectName;
  private final String projectSessionId;

  protected LogMessage(String message) {
    this.message = Objects.requireNonNull(message);
    this.projectId = CloudAPI.getCloudProjectId();
    var currentProject = CurrentEnsoProject.get();
    this.projectName = currentProject == null ? null : currentProject.fullName();
    this.projectSessionId = CloudAPI.getCloudSessionId();
  }

  private static void checkNoRestrictedField(ObjectNode metadata, String fieldName) {
    if (metadata.has(fieldName)) {
      throw new IllegalArgumentException(
          "Metadata cannot contain a field named '" + fieldName + "'. Metadata was: " + metadata);
    }
  }

  protected static void checkNoRestrictedFields(ObjectNode metadata) {
    checkNoRestrictedField(metadata, RESERVED_TYPE);
    checkNoRestrictedField(metadata, LOCAL_TIMESTAMP);
    checkNoRestrictedField(metadata, PROJECT_NAME);
    checkNoRestrictedField(metadata, PROJECT_ID);
  }

  private ObjectNode computedMetadata() {
    var meta = new ObjectNode(JsonNodeFactory.instance);
    meta.set(PROJECT_ID, projectId == null ? NullNode.getInstance() : TextNode.valueOf(projectId));
    // The project name may be null if a script is run outside a project.
    if (projectName != null) {
      meta.set(PROJECT_NAME, TextNode.valueOf(projectName));
    }

    if (projectSessionId != null) {
      meta.set(PROJECT_SESSION_ID, TextNode.valueOf(projectSessionId));
    }

    if (extraMetadata() != null) {
      extraMetadata()
          .fields()
          .forEachRemaining(
              entry -> {
                meta.set(entry.getKey(), entry.getValue());
              });
    }
    return meta;
  }

  public final String payload() {
    var payload = new ObjectNode(JsonNodeFactory.instance);
    payload.set("message", TextNode.valueOf(message));
    payload.set("metadata", computedMetadata());
    payload.set("kind", TextNode.valueOf(kind()));
    if (extraPayload() != null) {
      extraPayload()
          .fields()
          .forEachRemaining(
              entry -> {
                payload.set(entry.getKey(), entry.getValue());
              });
    }
    return payload.toString();
  }

  protected abstract String kind();

  /** Returns optional JSON object that will be appended to the payload. May return null. */
  protected abstract ObjectNode extraPayload();

  /** Returns optional JSON object that will be appended to the metadata. May return null. */
  protected abstract ObjectNode extraMetadata();

  @Override
  public final String toString() {
    return payload();
  }
}

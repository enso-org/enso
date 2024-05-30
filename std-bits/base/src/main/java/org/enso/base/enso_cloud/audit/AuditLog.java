package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.ObjectNode;

public final class AuditLog {
  public static void logAsync(String type, String message, ObjectNode metadata) {
    var event = new AuditLogMessage(type, message, metadata);
    AuditLogAPI.INSTANCE.logAsync(event);
  }

  public static void logSynchronously(String type, String message, ObjectNode metadata) {
    var event = new AuditLogMessage(type, message, metadata);
    AuditLogAPI.INSTANCE.logSync(event);
  }
}

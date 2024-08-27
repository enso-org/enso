package org.enso.base.enso_cloud.audit;

import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * The high-level API for logging audit events.
 * <p>
 * The messages are sent on a single background thread in batches, but also as soon as possible.
 * That means that we are never waiting for more messages to be batched to avoid delaying logs.
 * However, if sending the previous batch took enough time that many messages have been scheduled in the meantime,
 * all waiting messages (up to some limit) will be sent in a single request.
 */
public final class AuditLog {
  /**
   * Schedules the log message to be sent in the next batch, and returns immediately.
   */
  public static void logAsync(String type, String message, ObjectNode metadata) {
    var event = new AuditLogMessage(type, message, metadata);
    AuditLogApiAccess.INSTANCE.logWithoutConfirmation(event);
  }

  /**
   * Schedules the log message to be sent in the next batch, and waits until it has been sent.
   */
  public static void logSynchronously(String type, String message, ObjectNode metadata) {
    var event = new AuditLogMessage(type, message, metadata);
    Future<Void> future = AuditLogApiAccess.INSTANCE.logWithConfirmation(event);
    try {
      future.get();
    } catch (ExecutionException | InterruptedException e) {
      throw new RuntimeException("Exception occurred when sending an audit log message: " + e, e);
    }
  }
}

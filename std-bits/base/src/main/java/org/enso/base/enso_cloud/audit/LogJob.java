package org.enso.base.enso_cloud.audit;

import java.util.concurrent.CompletableFuture;

/**
 * A record that represents a single log to be sent.
 *
 * <p>It may contain the `completionNotification` future that will be completed when the log is
 * sent. If no-one is listening for confirmation, that field will be `null`.
 */
record LogJob(AuditLogApiAccess.LogMessage message, CompletableFuture<Void> completionNotification) {
}

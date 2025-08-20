package org.enso.logging.service.common;

import java.util.concurrent.CompletableFuture;

/**
 * Wrapper for {@link LogMessage} with an optional completable future for notification.
 *
 * @param message
 * @param completionNofitication May be null if no one wants to listen to the completion of the job.
 *     If not null, it will be completed when the job is finished.
 */
public record LogJob(LogMessage message, CompletableFuture<Void> completionNofitication) {}

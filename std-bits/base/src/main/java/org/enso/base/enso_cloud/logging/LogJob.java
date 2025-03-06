package org.enso.base.enso_cloud.logging;

import java.util.concurrent.CompletableFuture;

final class LogJob {
  private final LogMessage logMessage;
  private final CompletableFuture<Void> completionNotification;
  private final RequestConfig requestConfig;

  public LogJob(
      LogMessage logMessage,
      CompletableFuture<Void> completionNotification,
      RequestConfig requestConfig) {
    this.logMessage = logMessage;
    this.completionNotification = completionNotification;
    this.requestConfig = requestConfig;
  }

  public LogMessage getLogMessage() {
    return logMessage;
  }

  public CompletableFuture<Void> getCompletionNotification() {
    return completionNotification;
  }

  public RequestConfig getRequestConfig() {
    return requestConfig;
  }
}

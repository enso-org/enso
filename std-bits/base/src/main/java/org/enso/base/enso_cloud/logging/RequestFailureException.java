package org.enso.base.enso_cloud.logging;

public final class RequestFailureException extends RuntimeException {
  public RequestFailureException(String message, Throwable cause) {
    super(message, cause);
  }
}

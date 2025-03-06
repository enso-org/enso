package org.enso.base.enso_cloud.logging;

final class RequestFailureException extends RuntimeException {
  RequestFailureException(String message, Throwable cause) {
    super(message, cause);
  }
}

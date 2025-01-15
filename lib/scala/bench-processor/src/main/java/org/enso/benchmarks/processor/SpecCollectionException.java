package org.enso.benchmarks.processor;

/** An exception thrown when there is an issue collecting benchmark specifications. */
public final class SpecCollectionException extends RuntimeException {
  SpecCollectionException(String message) {
    super(message);
  }

  SpecCollectionException(String message, Throwable cause) {
    super(message, cause);
  }
}

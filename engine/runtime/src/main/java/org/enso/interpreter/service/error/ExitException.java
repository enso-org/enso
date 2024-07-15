package org.enso.interpreter.service.error;

/** Thrown when {@code System.exit} call was called during execution. */
public class ExitException extends RuntimeException implements ServiceException {
  public ExitException(int code) {
    super("Exit was called with exit code " + code + ".");
  }
}

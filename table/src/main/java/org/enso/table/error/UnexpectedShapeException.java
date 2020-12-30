package org.enso.table.error;

public class UnexpectedShapeException extends RuntimeException {
  /**
   * Creates a new instance of this error.
   *
   * @param expected the expected shape description
   * @param got description of shape of actual argument
   */
  public UnexpectedShapeException(String expected, String got) {
    super("Unexpected shape. Expected " + expected + ", but got " + got + ".");
  }
}

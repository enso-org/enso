package org.enso.table.error;

/** An error thrown when a type error is encountered. */
public class UnexpectedColumnTypeException extends RuntimeException {
  private final String expected;

  /**
   * Creates a new instance of this error.
   *
   * @param expected the expected type description
   */
  public UnexpectedColumnTypeException(String expected) {
    super("Unexpected column type. Expected a " + expected + " column.");
    this.expected = expected;
  }

  /**
   * @return the expected type descriptor
   */
  public String getExpected() {
    return expected;
  }
}

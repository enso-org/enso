package org.enso.table.error;

/** An error thrown when a type error is encountered. */
public class UnexpectedTypeException extends RuntimeException {
  private final String expected;

  /**
   * Creates a new instance of this error.
   *
   * @param expected the expected type description
   */
  public UnexpectedTypeException(String expected) {
    super("Unexpected value type. Expected " + expected + ".");
    this.expected = expected;
  }

  /** @return the expected type descriptor */
  public String getExpected() {
    return expected;
  }
}

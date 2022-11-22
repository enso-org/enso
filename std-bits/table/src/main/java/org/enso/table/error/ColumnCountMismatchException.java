package org.enso.table.error;

public class ColumnCountMismatchException extends Exception {
  private final int expected;
  private final int actual;

  public ColumnCountMismatchException(int expected, int actual) {
    super(String.format("Expected %d columns, got %d.", expected, actual));
    this.expected = expected;
    this.actual = actual;
  }

  public int getExpected() {
    return expected;
  }

  public int getActual() {
    return actual;
  }
}

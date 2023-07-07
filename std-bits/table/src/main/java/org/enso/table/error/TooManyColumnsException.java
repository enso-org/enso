package org.enso.table.error;

public class TooManyColumnsException extends RuntimeException {
  private final int columnCount;

  public TooManyColumnsException(String message, int columnCount) {
    super(message);
    this.columnCount = columnCount;
  }

  public int getColumnCount() {
    return columnCount;
  }
}

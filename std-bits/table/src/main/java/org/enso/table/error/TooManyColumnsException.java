package org.enso.table.error;

public class TooManyColumnsException extends RuntimeException {
  private final int columnCount;
  private final int maximumColumnCount;

  public TooManyColumnsException(String message, int columnCount, int maximumColumnCount) {
    super(message);
    this.columnCount = columnCount;
    this.maximumColumnCount = maximumColumnCount;
  }

  public int getColumnCount() {
    return columnCount;
  }

  public int getMaximumColumnCount() {
    return maximumColumnCount;
  }
}

package org.enso.table.data.table.problems;

public class InvalidAggregation implements Problem {
  private final String columnName;
  private final int row;
  private final String message;

  public InvalidAggregation(String columnName, int row, String message) {
    this.columnName = columnName;
    this.row = row;
    this.message = message;
  }

  public String getColumnName() {
    return columnName;
  }

  public int getRow() {
    return row;
  }

  @Override
  public String getMessage() { return message; }
}

package org.enso.table.data.table.problems;

public class FloatingPointGrouping implements Problem {
  private final String columnName;
  private final int row;

  public FloatingPointGrouping(String columnName, int row) {
    this.columnName = columnName;
    this.row = row;
  }

  public String getColumnName() {
    return columnName;
  }

  public int getRow() {
    return row;
  }

  @Override
  public String getMessage() {
    return this.getColumnName() + " " + this.row + " - grouping on floating points is not recommended.";
  }
}

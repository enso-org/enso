package org.enso.table.data.table.problems;

public class InvalidAggregation extends ColumnAggregatedProblem {
  private final String message;

  public InvalidAggregation(String columnName, int row, String message) {
    super(columnName, row);
    this.message = message;
  }

  @Override
  public String getMessage() {
    return message;
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    if (another instanceof InvalidAggregation
        && this.getLocationName().equals(another.getLocationName())
        && this.message.equals(((InvalidAggregation) another).message)) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }
}

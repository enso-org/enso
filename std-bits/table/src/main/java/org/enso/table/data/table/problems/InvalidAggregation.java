package org.enso.table.data.table.problems;

public class InvalidAggregation extends ColumnAggregatedProblems {
  private final String message;

  public InvalidAggregation(String columnName, int row, String message) {
    super(columnName, row);
    this.message = message;
  }

  @Override
  public String getMessage() { return message; }

  @Override
  public boolean merge(ColumnAggregatedProblems another) {
    if (another instanceof InvalidAggregation &&
        this.getColumnName().equals(another.getColumnName()) &&
        this.message.equals(((InvalidAggregation) another).message)) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }
}

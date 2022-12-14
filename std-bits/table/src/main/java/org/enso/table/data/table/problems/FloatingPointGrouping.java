package org.enso.table.data.table.problems;

public class FloatingPointGrouping extends ColumnAggregatedProblems {
  public FloatingPointGrouping(String columnName, int row) {
    super(columnName, row);
  }

  @Override
  public String getMessage() {
    return "Grouping on floating points is not recommended.";
  }

  @Override
  public boolean merge(ColumnAggregatedProblems another) {
    // We purposefully ignore merging `rows` because we do not use these on the result anyway.
    return another instanceof FloatingPointGrouping
        && this.getColumnName().equals(another.getColumnName());
  }
}

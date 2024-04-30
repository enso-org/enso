package org.enso.table.data.table.problems;

public class FloatingPointGrouping extends ColumnAggregatedProblem {

  public FloatingPointGrouping(String columnName, int row) {
    super(columnName, row);
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    // We purposefully ignore merging `rows` because we do not use these on the result anyway.
    return another instanceof FloatingPointGrouping
        && this.getLocationName().equals(another.getLocationName());
  }
}

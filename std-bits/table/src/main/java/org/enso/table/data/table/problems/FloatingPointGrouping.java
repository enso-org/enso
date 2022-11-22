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
    if (another instanceof FloatingPointGrouping
        && this.getColumnName().equals(another.getColumnName())) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }
}

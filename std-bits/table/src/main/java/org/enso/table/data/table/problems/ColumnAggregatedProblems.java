package org.enso.table.data.table.problems;

import java.util.ArrayList;
import java.util.List;

public abstract class ColumnAggregatedProblems implements Problem {
  private final String columnName;
  protected final List<Integer> rows;

  protected ColumnAggregatedProblems(String columnName, int row) {
    this.columnName = columnName;
    this.rows = new ArrayList<>();
    this.rows.add(row);
  }

  public String getColumnName() {
    return columnName;
  }

  public int[] getRows() {
    return rows.stream().mapToInt(Integer::intValue).toArray();
  }

  public int count() { return rows.size(); }

  public abstract boolean merge(ColumnAggregatedProblems another);

  public abstract String getMessage();
}

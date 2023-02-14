package org.enso.table.data.table.problems;

import org.enso.table.problems.Problem;

import java.util.ArrayList;
import java.util.List;

public abstract class ColumnAggregatedProblems implements Problem {
  private final String locationName;
  protected final List<Integer> rows;

  protected ColumnAggregatedProblems(String locationName, Integer row) {
    this.locationName = locationName;
    this.rows = new ArrayList<>();
    this.rows.add(row);
  }

  public String getLocationName() {
    return locationName;
  }

  public int[] getRows() {
    return rows.stream().mapToInt(Integer::intValue).toArray();
  }

  public int count() {
    return rows.size();
  }

  public abstract boolean merge(ColumnAggregatedProblems another);

  public abstract String getMessage();
}

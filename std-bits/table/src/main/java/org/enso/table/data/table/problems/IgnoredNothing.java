package org.enso.table.data.table.problems;

public class IgnoredNothing extends ColumnAggregatedProblem {

  public IgnoredNothing(String locationName, Integer row) {
    super(locationName, row);
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    if (another instanceof IgnoredNothing
        && this.getLocationName().equals(another.getLocationName())) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }
}

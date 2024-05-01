package org.enso.table.data.table.problems;

public class IgnoredNaN extends ColumnAggregatedProblem {

  public IgnoredNaN(String locationName, Integer row) {
    super(locationName, row);
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    if (another instanceof IgnoredNaN && this.getLocationName().equals(another.getLocationName())) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }

  @Override
  public String getMessage() {
    return "The column "
        + getLocationName()
        + " contained NaN values in rows "
        + makeTruncatedRowsString()
        + " which were ignored.";
  }
}

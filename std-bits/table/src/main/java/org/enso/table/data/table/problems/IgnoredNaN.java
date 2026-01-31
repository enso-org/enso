package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class IgnoredNaN extends ColumnAggregatedProblem {

  public IgnoredNaN(String locationName, long row) {
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

  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors", "Ignored_NaN_Values", "Warning", getLocationName(), getRows());
  }
}

package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class FloatingPointGrouping extends ColumnAggregatedProblem {
  public FloatingPointGrouping(String columnName, long row) {
    super(columnName, row);
  }

  @Override
  public String getMessage() {
    return "Checking exact equality of floating-point numbers is not recommended.";
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    // We purposefully ignore merging `rows` because we do not use these on the result anyway.
    return another instanceof FloatingPointGrouping
        && this.getLocationName().equals(another.getLocationName());
  }

  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Base.Errors.Common", "Floating_Point_Equality", "Error", getLocationName());
  }
}

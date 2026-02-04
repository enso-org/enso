package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class IllegalArgumentError extends ColumnAggregatedProblem {
  private final String message;

  public IllegalArgumentError(String locationName, String message, long row) {
    super(locationName, row);
    this.message = message;
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    if (another instanceof IllegalArgumentError IllegalArgumentError
        && this.getLocationName().equals(IllegalArgumentError.getLocationName())
        && this.message.equals(IllegalArgumentError.message)) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }

  @Override
  public String getMessage() {
    return message + " (at rows " + makeTruncatedRowsString() + ").";
  }

  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Base.Errors.Illegal_Argument", "Illegal_Argument", "Error", getMessage(), null);
  }
}

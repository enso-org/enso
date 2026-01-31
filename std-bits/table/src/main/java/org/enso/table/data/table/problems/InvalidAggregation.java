package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class InvalidAggregation extends ColumnAggregatedProblem {
  private final String message;

  public InvalidAggregation(String columnName, long row, String message) {
    super(columnName, row);
    this.message = message;
  }

  @Override
  public String getMessage() {
    return message;
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    if (another instanceof InvalidAggregation
        && this.getLocationName().equals(another.getLocationName())
        && this.message.equals(((InvalidAggregation) another).message)) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }

  @Override
  public boolean isError() {
    return true;
  }

  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Invalid_Aggregation",
        "Error",
        getLocationName(),
        getRows(),
        getMessage());
  }
}

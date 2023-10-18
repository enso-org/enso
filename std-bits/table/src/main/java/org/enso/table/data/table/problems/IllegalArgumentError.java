package org.enso.table.data.table.problems;

public class IllegalArgumentError extends ColumnAggregatedProblem {
  private final String message;

  public IllegalArgumentError(String locationName, String message, Integer row) {
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
}

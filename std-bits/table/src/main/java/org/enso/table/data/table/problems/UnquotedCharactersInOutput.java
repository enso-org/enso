package org.enso.table.data.table.problems;

public class UnquotedCharactersInOutput extends ColumnAggregatedProblems {
  public UnquotedCharactersInOutput(String columnName, int row) {
    super(columnName, row);
  }

  @Override
  public String getMessage() {
    return "The "
        + getLocationName()
        + " at rows "
        + makeTruncatedRowsString()
        + " contains characters that need quoting, but quoting is disabled. The generated file may be corrupted.";
  }

  @Override
  public boolean merge(ColumnAggregatedProblems another) {
    if (another instanceof UnquotedCharactersInOutput
        && this.getLocationName().equals(another.getLocationName())) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }
}

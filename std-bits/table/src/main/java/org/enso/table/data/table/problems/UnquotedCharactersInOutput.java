package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public class UnquotedCharactersInOutput extends ColumnAggregatedProblem {
  public UnquotedCharactersInOutput(String columnName, long row) {
    super(columnName, row);
  }

  @Override
  public String getMessage() {
    return "The "
        + getLocationName()
        + " at rows "
        + makeTruncatedRowsString()
        + " contains characters that need quoting, but quoting is disabled. The generated file may"
        + " be corrupted.";
  }

  @Override
  public boolean merge(ColumnAggregatedProblem another) {
    if (another instanceof UnquotedCharactersInOutput
        && this.getLocationName().equals(another.getLocationName())) {
      this.rows.addAll(another.rows);
      return true;
    }

    return false;
  }

  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Unquoted_Characters_In_Output",
        "Warning",
        getLocationName(),
        getRows());
  }
}

package org.enso.table.error;

import java.util.Arrays;

public class ColumnNameMismatchException extends Exception {
  private final String[] missing;
  private final String[] extras;

  public ColumnNameMismatchException(String[] missingNames, String[] extraNames) {
    super(
        "Columns mismatch."
            + (missingNames.length == 0 ? "" : " Missing: " + Arrays.toString(missingNames))
            + (extraNames.length == 0 ? "" : " Extra: " + Arrays.toString(extraNames)));

    this.missing = missingNames;
    this.extras = extraNames;
  }

  public String[] getMissing() {
    return missing;
  }

  public String[] getExtras() {
    return extras;
  }
}

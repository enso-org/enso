package org.enso.tableau;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

class HyperUnmatchedColumns extends RuntimeException {
  private final String[] unmatchedColumns;

  public HyperUnmatchedColumns(String[] unmatchedColumns) {
    super("Table contains unexpected columns: " + String.join(", ", unmatchedColumns));
    this.unmatchedColumns = unmatchedColumns;
  }

  public String[] getUnmatchedColumns() {
    return unmatchedColumns;
  }

  public Value asEnsoAtom() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors", "Unmatched_Columns", "Error", (Object) getUnmatchedColumns());
  }
}

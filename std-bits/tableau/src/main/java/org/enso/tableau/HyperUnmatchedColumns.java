package org.enso.tableau;

public class HyperUnmatchedColumns extends RuntimeException {
  private final String[] unmatchedColumns;

  public HyperUnmatchedColumns(String[] unmatchedColumns) {
    super("Table contains unexpected columns: " + String.join(", ", unmatchedColumns));
    this.unmatchedColumns = unmatchedColumns;
  }

  public String[] getUnmatchedColumns() {
    return unmatchedColumns;
  }
}

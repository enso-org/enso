package org.enso.table.read;

/** A problem indicating that a row contained more or less columns than expected. */
public class InvalidRow implements ParsingProblem {
  public final long source_row;
  public final Long table_index;
  public final String[] row;

  public InvalidRow(long source_row, Long table_index, String[] row) {
    this.source_row = source_row;
    this.table_index = table_index;
    this.row = row;
  }
}

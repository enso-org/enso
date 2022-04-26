package org.enso.table.read;

public class InvalidRow extends RuntimeException {
  public final long index;
  public final String[] row;

  public InvalidRow(long index, String[] row) {
    this.index = index;
    this.row = row;
  }
}

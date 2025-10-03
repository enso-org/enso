package org.enso.table.read;

import org.enso.base.encoding.DecodingProblemAggregator;

public class FixedWidthDecodingProblemAggregator
    extends DecodingProblemAggregator<FixedWidthDecodingProblemAggregator.Location> {
  private long row = -1;
  private int column = -1;

  public void setRowColumn(long row, int column) {
    this.row = row;
    this.column = column;
  }

  @Override
  protected Location toLocation(int position) {
    return new Location(row, column, position);
  }

  public record Location(long row, int column, int position) {
    @Override
    public String toString() {
      String rowExp = row == -1 ? "unknown" : Long.toString(row);
      String columnExp = column == -1 ? "unknown" : Integer.toString(column);
      return "character " + position + " row " + rowExp + " column " + columnExp;
    }
  }
}

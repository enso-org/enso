package org.enso.table.data.column.builder;

import org.enso.table.problems.Problem;

/** Indicates that an integer being converted to double cannot be represented precisely. */
public class LossOfIntegerPrecision implements Problem {
  private final long exampleValue;
  private final double exampleValueConverted;
  private long affectedRows;

  public LossOfIntegerPrecision(long exampleValue, double exampleValueConverted) {
    this.exampleValue = exampleValue;
    this.exampleValueConverted = exampleValueConverted;
    this.affectedRows = 1;
  }

  public long getExampleValue() {
    return exampleValue;
  }

  public double getExampleValueConverted() {
    return exampleValueConverted;
  }

  public long getAffectedRowsCount() {
    return affectedRows;
  }

  void incrementAffectedRows() {
    affectedRows++;
  }
}

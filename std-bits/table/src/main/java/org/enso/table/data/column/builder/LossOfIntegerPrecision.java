package org.enso.table.data.column.builder;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** Indicates that an integer being converted to double cannot be represented precisely. */
public class LossOfIntegerPrecision implements Problem {
  private final Number exampleValue;
  private final double exampleValueConverted;
  private long affectedRows;

  public LossOfIntegerPrecision(Number exampleValue, double exampleValueConverted) {
    this.exampleValue = exampleValue;
    this.exampleValueConverted = exampleValueConverted;
    this.affectedRows = 1;
  }

  public Number getExampleValue() {
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

  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Loss_Of_Integer_Precision",
        "Warning",
        getAffectedRowsCount(),
        getExampleValue(),
        getExampleValueConverted());
  }
}
